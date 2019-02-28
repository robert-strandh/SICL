(cl:in-package #:cleavir-generate-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Managing top-level forms.
;;;
;;; We need to be able to test whether a form is a top-level form or
;;; not.  A few special forms (LOCALLY, MACROLET, SYMBOL-MACROLET)
;;; preserve this property in that if the special form itself is a
;;; top-level form, then the body of the special form is also a
;;; top-level form.  For all other forms, any subform of the form is
;;; not considered a top-level form.

;;; The reason for the following somewhat twisted logic is that we
;;; want to avoid having to explicitly set *SUBFORMS-ARE-TOP-LEVEL-P*
;;; to false in every method EXCEPT the ones for LOCALLY, MACROLET,
;;; and SYMBOL-MACROLET.  This logic allows us to add some code ONLY
;;; to these special forms in order to indicate that they preserve the
;;; top-level property.
;;;
;;; The way this logic works is as follows: We define a second
;;; variable named *CURRENT-FORM-IS-TOP-LEVEL-P*.  This variable holds
;;; the value of *SUBFORMS-ARE-TOP-LEVEL-P* as it was before CONVERT was
;;; called, and this is the variable that we actually test in order to
;;; determine whether a form is a top-level form.  To obtain that, we
;;; define an :AROUND method on CONVERT that binds
;;; *CURRENT-FORM-IS-TOP-LEVEL-P* to the value of *SUBFORMS-ARE-TOP-LEVEL-P*
;;; for the duration of the invocation of the primary method on
;;; CONVERT, and that also binds *SUBFORMS-ARE-TOP-LEVEL-P* to false.  Any
;;; recursive invocation of CONVERT will thus automatically see the
;;; value of *CURRENT-FORM-IS-TOP-LEVEL-P* as false.  The methods for
;;; LOCALLY, MACROLET, and SYMBOL-MACROLET set
;;; *CURRENT-FORM-IS-TOP-LEVEL-P* to true so that when they
;;; recursively call CONVERT, then this true value will be the value
;;; of *CURRENT-FORM-IS-TOP-LEVEL-P*.  I hope this explanation makes
;;; sense.

;;; This variable is true if and only if the current form is a
;;; top-level form.
(defvar *current-form-is-top-level-p*)

;;; This variable is true if and only if the subforms of the current
;;; form are top-level forms.
(defvar *subforms-are-top-level-p*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variables that control certain behavior of the compiler.

;;; This variable should be bound by client code to one of the symbols
;;; CL:COMPILE, CL:COMPILE-FILE, or CL:EVAL before the main entry
;;; point is called.
(defvar *compiler*)

;;; This variable indicates whether a form should be evaluated in
;;; addition to be being processed by the compiler.
(defvar *compile-time-too*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting code to an abstract syntax tree.

;;; The main entry point for converting a form.
(defgeneric convert (form environment system))

;;; Utility function for converting a sequence of forms, represented
;;; as a list.
(defun convert-sequence (forms environment system)
  (loop for form in forms
	collect (convert form environment system)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Turn a list of ASTs into either a PROGN-AST or a
;;; LOAD-TIME-VALUE-AST containing NIL in case the list of ASTs is
;;; NIL.

(defun process-progn (asts)
  (if (null asts)
      (cleavir-ast:make-load-time-value-ast 'nil t)
      (cleavir-ast:make-progn-ast asts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONVERT-CONSTANT is called when a constant is found, either in the
;;; form of a literal or in the form of a constant variable.
;;;
;;; The result of the conversion can be either an IMMEDIATE-AST if the
;;; constant is such that it can be represented as an immediate value
;;; in the resulting machine code, or it can be a LOAD-TIME-VALUE-AST
;;; if it can not be represented as an immediate.
;;;
;;; Obviously, Cleavir can not know a priori neither whether the
;;; constant can be represented as an immediate value, nor can it know
;;; in that case what the corresponding immediate representation is.
;;; For that reason, we first call the generic function named
;;; CONVERT-CONSTANT-TO-IMMEDIATE with the constant value and the
;;; environment.  The function CONVERT-CONSTANT-TO-IMMEDIATE may
;;; return NIL, meaning that this constant does not have a
;;; representation as an immediate value, or it may return a
;;; possibly-negative integer which is taken to be the representation
;;; of the constant as an immediate machine word.  A default method is
;;; provided that always returns NIL.

(defgeneric convert-constant-to-immediate (constant env system))

(defmethod convert-constant-to-immediate (constant env system)
  (declare (ignore constant env system))
  nil)

(defun convert-constant (constant env system)
  (let* ((global-env (cleavir-env:global-environment env))
	 (immediate (convert-constant-to-immediate
		     constant global-env system)))
    (if (null immediate)
	(cleavir-ast:make-load-time-value-ast `',constant t)
	(cleavir-ast:make-immediate-ast immediate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a compound form when the head of a compound form is a
;;; CONS.  Then the head must be a lambda expression.  We replace a
;;; call such as ((lambda (params) . body) . args) by (flet ((temp
;;; (params) . body)) (temp . args))
;;;
;;; FIXME: do some more error checking.
(defgeneric convert-lambda-call (form env system))

(defmethod convert-lambda-call (form env system)
  (destructuring-bind ((lambda lambda-list &rest body) &rest args) form
    (assert (eql lambda 'cl:lambda) nil
            'lambda-call-first-symbol-not-lambda :expr lambda)
    (cleavir-ast:make-call-ast
     (convert-code lambda-list body env system)
     (convert-sequence args env system))))

;;; The general method for processing the lambda list is as follows:
;;; We use recursion to process the remaining lambda list.  Before the
;;; recursive call, we add the current parameters to the environment
;;; that we pass to the recursive call.  The call returns two values:
;;; the AST that was built and modified lambda list, containing the
;;; lambda list keywords, and the lexical variables that were
;;; introduced.  The exception is that processing &AUX entries does
;;; not return any lambda list, because it will always be empty.
;;;
;;; The reason we do it this way is that, if a parameter turns out to
;;; be a special variable, the entire rest of the lambda list and
;;; function body must be executed with this variable bound.  The AST
;;; configuration for expressing that situation is that the AST for
;;; computing the rest of the lambda list and the body must be a child
;;; of a BIND-AST that indicates that the special variable should be
;;; bound.  This recursive method makes sure that the child exists
;;; before the BIND-AST is created.
;;;
;;; The parameter DSPECS that is used in several functions is a list
;;; of canonicalized declaration specifiers.  This list is used to
;;; determine whether a variable is declared special.

;;; This class is used to describe the body of a function.  It
;;; contains the declaration specifiers that apply to the body as a
;;; whole, the forms of the body and information about a possible
;;; BLOCK that the body code should be wrapped in.  The main reason
;;; for the existence of this class is to keep the number of arguments
;;; down of several functions below, not for the purpose of
;;; performance, but simply to avoid very long lambda lists in the
;;; source code.
(defclass body ()
  ((%dspecs :initarg :dspecs :accessor dspecs)
   (%forms :initarg :forms :accessor forms)
   (%block-name :initarg :block-name :reader block-name)
   (%block-name-p :initarg :block-name-p :reader block-name-p)))

(defun make-body (dspecs forms block-name block-name-p)
  (make-instance 'body
    :dspecs dspecs
    :forms forms
    :block-name block-name
    :block-name-p block-name-p))

;;; Convert the body of a function.
(defun convert-body (body env system)
  (let ((new-env (augment-environment-with-declarations env (dspecs body))))
    (convert (if (block-name-p body)
		 `(block ,(block-name body) ,@(forms body))
		 `(progn ,@(forms body)))
	     new-env system)))

;;; We have already detected there is an &AUX lambda-list keyword in
;;; the lambda list, and this function recursively processes the
;;; remaining &AUX "parameters".
(defun process-remaining-aux (aux idspecs body env system)
  (if (null aux)
      ;; We ran out of &AUX "parameters".  We must build an AST for
      ;; the body of the function.
      (convert-body body env system)
      ;; We have at least one more &AUX "parameter".
      (destructuring-bind (var init) (first aux)
	(let (;; We enter the new parameter variable into the
              ;; environment in order to process remaining parameters
              ;; and ultimately the body of the function.
              (new-env (augment-environment-with-variable
                        var (first idspecs) env env))
              ;; The initform of the &AUX parameter is turned into an
              ;; AST in the original environment, i.e. the one that
              ;; does not have the parameter variable in it.
              (value-ast (convert init env system)))
          (set-or-bind-variable var value-ast
                                (lambda ()
                                  (process-remaining-aux
                                   (rest aux) (rest idspecs)
                                   body new-env system))
                                new-env system)))))

;;; This function is called when we have processed all the &KEY
;;; parameters, so if there are any &AUX "parameters", they should be
;;; processed by this function.
(defun process-aux (parsed-lambda-list idspecs body env system)
  (let ((aux (cleavir-code-utilities:aux parsed-lambda-list)))
    (if (eq aux :none)
	;; This lambda list has no &AUX "parameters".  We must build
	;; an AST for the body of the function.
	(convert-body body env system)
	;; This lambda list has the &AUX keyword in it.  There may or
	;; may not be any &AUX "parameters" following that keyword.
	;; We call PROCESS-REMAINING-AUX with the list of these &AUX
	;; "parameters", and we return the AST that
	;; PROCESS-REMAINING-AUX builds.
	(process-remaining-aux aux idspecs body env system))))

;;; VAR-AST and SUPPLIED-P-AST are LEXICAL-ASTs that will be set by
;;; the implementation-specific argument-parsing code, according to
;;; what arguments were given.  VALUE-AST is an AST that computes the
;;; initialization for of the variable.  This function generates the
;;; code for testing whether SUPPLIED-P-AST computes NIL or T, and for
;;; assigning the value computed by VALUE-AST to VAR-AST if
;;; SUPPLIED-P-AST computes NIL.
(defun generate-initialization
    (var-ast supplied-p-ast value-ast env system)
  (cleavir-ast:make-if-ast
   (cleavir-ast:make-eq-ast
    supplied-p-ast
    (convert-constant nil env system))
   (cleavir-ast:make-setq-ast var-ast value-ast)
   (convert-constant nil env system)))

(defun process-init-parameter
    (var lvar supplied-p splvar init-ast env next-thunk system)
  (process-progn
   (list (generate-initialization lvar splvar init-ast env system)
         (set-or-bind-variable
          var lvar (if (null supplied-p)
                       next-thunk
                       (lambda ()
                         (set-or-bind-variable
                          supplied-p splvar next-thunk env system)))
          env system))))

;;; The only purpose of this function is to call the function
;;; AUGMENT-ENVIRONMENT-WITH-VARIABLE twice, once for the parameter
;;; variable and once for its associated supplied-p parameter, except
;;; that it also tests whether the supplied-p parameter is NIL,
;;; indicating that no supplied-p parameter was given.  This function
;;; returns the augmented environment.
(defun augment-environment-with-parameter (var supplied-p dspecs env)
  (let ((new-env (augment-environment-with-variable
		  var dspecs env env)))
    (if (null supplied-p)
	new-env
	(augment-environment-with-variable supplied-p dspecs new-env new-env))))

(defun process-remaining-keys (keys parsed-lambda-list
                               idspecs body env system
                               key-lvars)
  (if (null keys)
      (process-aux parsed-lambda-list idspecs body env system)
      (destructuring-bind ((keyword var) init &optional supplied-p)
          (first keys)
        (declare (ignore keyword))
        (let ((new-env (augment-environment-with-parameter
                        var supplied-p (first idspecs) env))
              (init-ast (convert init env system))
              (key-lvar (first key-lvars)))
          (process-init-parameter
           var (first key-lvar) supplied-p (second key-lvar)
           init-ast new-env
           (lambda ()
             (process-remaining-keys
              (rest keys) parsed-lambda-list (rest idspecs)
              body new-env system (rest key-lvars)))
           system)))))

(defun process-keys (parsed-lambda-list idspecs body env system
                     key-lvars)
  (let ((keys (cleavir-code-utilities:keys parsed-lambda-list)))
    (if (eq keys :none)
        (process-aux parsed-lambda-list idspecs body env system)
        (process-remaining-keys keys parsed-lambda-list
                                idspecs body env system
                                key-lvars))))

(defun process-rest (parsed-lambda-list idspecs body env system
                     rest-lvar key-lvars)
  (let ((rest (cleavir-code-utilities:rest-body parsed-lambda-list)))
    (if (eq rest :none)
        (process-keys parsed-lambda-list idspecs body env system
                      key-lvars)
        (let ((new-env (augment-environment-with-variable
                        rest (first idspecs) env env)))
          (set-or-bind-variable
           rest rest-lvar
           (lambda ()
             (process-keys parsed-lambda-list (rest idspecs)
                           body new-env system key-lvars))
           new-env system)))))

(defun process-remaining-optionals
    (optionals parsed-lambda-list idspecs body env system
     optional-lvars rest-lvar key-lvars)
  (if (null optionals)
      (process-rest parsed-lambda-list idspecs body env system
                    rest-lvar key-lvars)
      (destructuring-bind (var init &optional supplied-p)
          (first optionals)
        (let ((new-env (augment-environment-with-parameter
                        var supplied-p (first idspecs) env))
              (init-ast (convert init env system))
              (optional-lvar (first optional-lvars)))
          (process-init-parameter
           var (first optional-lvar) supplied-p (second optional-lvar)
           init-ast new-env
           (lambda ()
             (process-remaining-optionals
              (rest optionals) parsed-lambda-list (rest idspecs)
              body new-env system (rest optional-lvars)
              rest-lvar key-lvars))
           system)))))

(defun process-optionals (parsed-lambda-list idspecs body env system
                          optional-lvars rest-lvar key-lvars)
  (let ((optionals (cleavir-code-utilities:optionals parsed-lambda-list)))
    (if (eq optionals :none)
        (process-rest parsed-lambda-list idspecs body env system
                      rest-lvar key-lvars)
        (process-remaining-optionals optionals parsed-lambda-list
                                     idspecs body env system
                                     optional-lvars rest-lvar
                                     key-lvars))))

(defun process-required (required parsed-lambda-list idspecs body
                         env system required-lvars optional-lvars
                         rest-lvar key-lvars)
  (if (null required)
      (process-optionals parsed-lambda-list idspecs body
                         env system optional-lvars
                         rest-lvar key-lvars)
      (let* ((var (first required))
             (lvar (first required-lvars))
             (new-env (augment-environment-with-variable
                       var (first idspecs) env env)))
        (set-or-bind-variable
         var lvar
         (lambda ()
           (process-required (rest required)
                             parsed-lambda-list
                             (rest idspecs)
                             body new-env system
                             (rest required-lvars) optional-lvars
                             rest-lvar key-lvars))
         new-env system))))

(defun make-lvar-name (symbol)
  (make-symbol (string-downcase symbol)))

(defun make-init-lvars (var supplied-p)
  (list
   (cleavir-ast:make-lexical-ast (make-lvar-name var))
   (cleavir-ast:make-lexical-ast
    (if (null supplied-p)
        (gensym)
        (make-lvar-name supplied-p)))))

(defun process-lambda-list (parsed-lambda-list idspecs body env system)
  ;;; First, make a lexical location for every variable. This is to let
  ;;; special bindings be irrelevant to body code. While we're at it we
  ;;; construct the eventual  lambda-list used in the function-ast.
  (let* ((required (cleavir-code-utilities:required parsed-lambda-list))
         (optionals (cleavir-code-utilities:optionals parsed-lambda-list))
         (rest-name '&rest)
         (rest (cleavir-code-utilities:rest-body parsed-lambda-list))
         (keys (cleavir-code-utilities:keys parsed-lambda-list))
         (aux (cleavir-code-utilities:aux parsed-lambda-list))
         (required-lvars (mapcar (lambda (var)
                                   (cleavir-ast:make-lexical-ast
                                    (make-lvar-name var)))
                                 required))
         (required-ll required-lvars)
         (optional-lvars (if (eq optionals :none)
                             nil
                             (mapcar
                              (lambda (spec)
                                (destructuring-bind (var init &optional supplied-p)
                                    spec
                                  (declare (ignore init))
                                  (make-init-lvars var supplied-p)))
                              optionals)))
         ;; (&optional (var-lex-ast supplied-p-lex-ast)+)
         (optional-ll (if (eq optionals :none) nil (cons '&optional optional-lvars)))
         (rest-lvar (if (eq rest :none) ; no rest parameter
                        nil
                        (cleavir-ast:make-lexical-ast
                         (make-lvar-name rest))))
         (rest-ll (if (eq rest :none) nil (list rest-name rest-lvar)))
         (key-lvars (if (eq keys :none)
                        nil
                        (mapcar
                         (lambda (spec)
                           (destructuring-bind ((keyword var) init &optional supplied-p)
                               spec
                             (declare (ignore keyword init))
                             (make-init-lvars var supplied-p)))
                         keys)))
         ;; (&key (keyword var-lex-ast supplied-p-lex-ast)+)
         (key-ll (if (eq keys :none)
                     nil
                     (cons '&key
                           (mapcar (lambda (spec lvars)
                                     (cons (caar spec) lvars))
                                   keys key-lvars))))
         (allow-other-keys-ll (if (cleavir-code-utilities:allow-other-keys
                                   parsed-lambda-list)
                                  '(&allow-other-keys)
                                  nil))
         ;; aux variables aren't in the lambda list, so we can defer making
         ;; their lvars.
         (lexical-lambda-list (append required-ll optional-ll rest-ll key-ll
                                      allow-other-keys-ll)))
    (values 
     (process-required required parsed-lambda-list idspecs body
                       env system required-lvars optional-lvars
                       rest-lvar key-lvars)
     lexical-lambda-list)))

;;; Given a parsed lambda list, return a list of items.  There are as
;;; many items in the list as there are bindings in the lambda list.
;;; In this case, an occurrence of a parameter together with a
;;; supplied-p parameter is considered to be a single item.  Each item
;;; is a list of one or two variables.  For a parameter with an
;;; associated supplied-p parameter, the item contains both the
;;; parameter and the associated supplied-p parameter.  Otherwise, the
;;; item contains just the parameter.
(defun itemize-lambda-list (parsed-lambda-list)
  (let ((required (cleavir-code-utilities:required parsed-lambda-list))
	(optionals (cleavir-code-utilities:optionals parsed-lambda-list))
	(rest-body (cleavir-code-utilities:rest-body parsed-lambda-list))
	(keys (cleavir-code-utilities:keys parsed-lambda-list))
	(aux (cleavir-code-utilities:aux parsed-lambda-list))
	(result '()))
    (unless (eq aux :none)
      (setf result (append (mapcar #'list (mapcar #'first aux)) result)))
    (unless (eq keys :none)
      (loop for key in (reverse keys)
	    do (push (if (= (length key) 3)
			 (list (second (first key)) (third key))
			 (list (second (first key))))
		     result)))
    (unless (eq rest-body :none)
      (push (list rest-body)
	    result))
    (unless (eq optionals :none)
      (loop for optional in (reverse optionals)
	    do (push (if (= (length optional) 3)
			 (list (first optional) (third optional))
			 (list (first optional)))
		     result)))
    (append (mapcar #'list required) result)))

(defgeneric convert-code (lambda-list body env system &key block-name))

(defmethod convert-code (lambda-list body env system
			 &key (block-name nil block-name-p))
  (let* ((parsed-lambda-list
	   (cleavir-code-utilities:parse-ordinary-lambda-list lambda-list)))
    (multiple-value-bind (declarations documentation forms)
	(cleavir-code-utilities:separate-function-body body)
      ;; FIXME: Handle documentation
      (declare (ignore documentation))
      (let ((canonicalized-dspecs
	      (cleavir-code-utilities:canonicalize-declaration-specifiers
	       (reduce #'append (mapcar #'cdr declarations))
               (cleavir-env:declarations env)))
            ;; make a fresh dynamic environment. The body as well as the
            ;; lambda list parsing code must be compiled with it.
            (cleavir-ast:*dynamic-environment*
              (cleavir-ast:make-dynamic-environment-ast
               '#:dynamic-environment-argument)))
	(multiple-value-bind (idspecs rdspecs)
	    (itemize-declaration-specifiers
	     (itemize-lambda-list parsed-lambda-list)
	     canonicalized-dspecs)
	  (multiple-value-bind (ast lexical-lambda-list)
	      (process-lambda-list
               parsed-lambda-list idspecs
               (make-body rdspecs forms block-name block-name-p)
               env system)
	    (cleavir-ast:make-function-ast
             ast lexical-lambda-list cleavir-ast:*dynamic-environment*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONVERT-FORM is called by CONVERT when the form is either a symbol
;;; or a CONS with a symbol in the CAR.  It is called with three
;;; arguments: 1. the form;  2. the environment INFO resulting from
;;; calling VARIABLE-INFO on the symbol or from calling FUNCTION-INFO
;;; on the CAR of the CONS;  3. the environment.

(defgeneric convert-form (form info environment system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONVERT is responsible for converting a form to an abstract syntax
;;; tree.

(defmethod convert (form environment system)
  (cond ((and (not (consp form)) (not (symbolp form)))
	 (convert-constant form environment system))
	((and (symbolp form) (constantp form))
	 (convert-constant (symbol-value form) environment system))
	((symbolp form)
	 (let ((info (variable-info environment form)))
	   (convert-form form info environment system)))
	((symbolp (car form))
	 ;; Even if we are in COMPILE-TIME-TOO mode, at this point, we
	 ;; do not know whether to evaluate the form at compile time,
	 ;; simply because it might be a special form that is handled
	 ;; specially.  So we must wait until we have more
	 ;; information.
	 (let ((info (function-info environment (car form))))
	   (convert-form form info environment system)))
	(t
	 ;; The form must be a compound form where the CAR is a lambda
	 ;; expression.  Evaluating such a form might have some
	 ;; compile-time side effects, so we must check whether we are
	 ;; in COMPILE-TIME-TOO mode, in which case we must evaluate
	 ;; the form as well.
	 (when (and *current-form-is-top-level-p* *compile-time-too*)
	   (cleavir-env:eval form environment environment))
	 (convert-lambda-call form environment system))))

(defun generate-ast (form environment system
                          &optional (cleavir-ast:*dynamic-environment*
                                     (cleavir-ast:make-dynamic-environment-ast
                                      '#:unused-dynamic-environment
                                      :policy (cleavir-env:environment-policy
                                               environment))))
  (let ((*subforms-are-top-level-p* t)
	(*compile-time-too* nil))
    (convert form environment system)))

(defmacro with-preserved-toplevel-ness (&body body)
  `(progn (setf *subforms-are-top-level-p* *current-form-is-top-level-p*)
	  ,@body))

(defmethod convert :around (form environment system)
  (declare (ignore system))
  (let ((*current-form-is-top-level-p* *subforms-are-top-level-p*)
	(*subforms-are-top-level-p* nil)
	;; gives all generated ASTs the appropriate policy.
	(cleavir-ast:*policy*
	  (cleavir-env:environment-policy environment)))
    (call-next-method)))
