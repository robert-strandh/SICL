(cl:in-package #:cleavir-cst-to-ast)

;;;; The general method for processing the lambda list is as follows:
;;;; We use recursion to process the remaining lambda list.  Before
;;;; the recursive call, we add the current parameters to the
;;;; environment that we pass to the recursive call.  The call returns
;;;; two values: the AST that was built and a modified lambda list,
;;;; containing the lambda list keywords, and the lexical variables
;;;; that were introduced.  The exception is that processing &AUX
;;;; entries does not return any lambda list, because it will always
;;;; be empty.
;;;;
;;;; The reason we do it this way is that, if a parameter turns out to
;;;; be a special variable, the entire rest of the lambda list and
;;;; function body must be executed with this variable bound.  The AST
;;;; configuration for expressing that situation is that the AST for
;;;; computing the rest of the lambda list and the body must be a
;;;; child of a BIND-AST that indicates that the special variable
;;;; should be bound.  This recursive method makes sure that the child
;;;; exists before the BIND-AST is created.
;;;;
;;;; The parameter DSPECS that is used in several functions is a list
;;;; of canonicalized declaration specifiers.  This list is used to
;;;; determine whether a variable is declared special.

;;;; Several functions in this system create a LEXICAL LAMBDA LIST.
;;;; Such a lambda list is similar to an ordinary lambda list.  It can
;;;; have required parameter, &OPTIONAL parameters, a &REST parameter,
;;;; and &KEY parameters.  It can not have any &AUX parameters,
;;;; though.  It can also have implementation-specific parameters.
;;;; The parameters are different from those of an ordinary lambda
;;;; list.  A required parameter is represented as a LEXICAL-AST
;;;; corresponding to the variable of the parameter in the original
;;;; lambda list.  The same thing is true for a &REST parameter.  An
;;;; &OPTIONAL parameter is represented as a list of two LEXICAL-ASTs.
;;;; The first AST of the list corresponds to the variable of the
;;;; parameter in the original lambda list, and the second AST
;;;; corresponds to a SUPPLIED-P parameter, whether the original
;;;; lambda list had such a parameter or not.  We generate ASTs that
;;;; test the SUPPLIED-P parameter and, if it is NIL, compute the
;;;; value of the initialization form if the corresponding parameter.
;;;; A &KEY parameter is represented as a list of three elements.  The
;;;; first element is the keyword to be used to determine whether this
;;;; parameter was given.  The remaining two elements play the same
;;;; role as those of an &OPTIONAL parameter.  We are not concerned
;;;; here with they way in which it is determined whether a particular
;;;; &KEY parameter was given or not.  This logic is determined for
;;;; each implementation.

;;; Process a single parameter.  This function first computes a new
;;; environment by augmenting ENVIRONMENT with information from
;;; PARAMETER.  Then it recursively processes the parameters in
;;; REMAINING-PARAMETERS-IN-GROUP and in REMAINING-PARAMETER-GROUPS in
;;; the augmented environment.  Finally, it returns two values.  The
;;; first return value is the AST resulting from the recursive
;;; processing, from the processing of PARAMETER and from BODY.  The
;;; second value is a lexical lambda list resulting from the
;;; processing of the parameters in PARAMETER, in
;;; REMAINING-PARAMETERS-IN-GROUP, and in REMAINING-PARAMETER-GROUPS.
(defgeneric process-parameter
    (parameter
     remaining-parameters-in-group
     remaining-parameter-groups
     idspecs
     body
     environment
     system))

;;; Process all the parameters in the list PARAMETERS-IN-GROUP.  This
;;; function first computes a new environment by augmenting
;;; ENVIRONMENT with information from the parameters in the list
;;; PARAMETERS-IN-GROUP.  Then it recursively processes the parameters
;;; in REMAINING-PARAMETER-GROUPS in the augmented environment.
;;; Finally, it returns two values.  The first return value is the AST
;;; resulting from the recursive processing and from the processing of
;;; the parameters in PARAMETERS-IN-GROUP and of BODY.  The second
;;; return value is a lexical lambda list corresponding to the
;;; parameters in PARAMETERS-IN-GROUP and in
;;; REMAINING-PARAMETER-GROUPS.
(defgeneric process-parameters-in-group
    (parameters-in-group
     remaining-parameter-groups
     idspecs
     body
     environment
     system))

;;; This function first computes a new environment by augmenting
;;; ENVIRONMENT with information from the parameters in
;;; PARAMETER-GROUP.  Then it recursively processes the parameters in
;;; REMAINING-PARAMETER-GROUPS in the augmented environment.  Finally,
;;; it returns two values.  The first return value is the AST
;;; resulting from the recursive processing and from the processing of
;;; the parameters in PARAMETER-GROUP and of BODY.  The second value
;;; is a lexical lambda list that corresponds to the parameters in
;;; PARAMETER-GROUP and REMAINING-PARAMETER-GROUPS.
(defgeneric process-parameter-group
    (parameter-group
     remaining-parameter-groups
     idspecs
     body
     environment
     system))

;;; Process all the parameters in the list of parameter groups
;;; PARAMETER-GROUPS.  This function returns two values.  The first
;;; return value is a list if parameter gruops that mirror the
;;; parameter groups in PARMETER-GROUPS.  The second return value is
;;; the AST resulting from the processing of those parameters and of
;;; BODY.
(defgeneric process-parameter-groups
    (parameter-groups
     idspecs
     body
     envrironment
     system))

(defmethod process-parameters-in-group
    ((parameters-in-group null)
     remaining-parameter-groups
     idspecs
     body
     environment
     system)
  (process-parameter-groups remaining-parameter-groups
                            idspecs
                            body
                            environment
                            system))

(defmethod process-parameters-in-group
    ((parameters-in-group cons)
     remaining-parameter-groups
     idspecs
     body
     environment
     system)
  (process-parameter (car parameters-in-group)
                     (cdr parameters-in-group)
                     remaining-parameter-groups
                     idspecs
                     body
                     environment
                     system))

(defgeneric new-environment-from-parameter
    (parameter idspecs environment system))

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
   ;; This slot contains an ordinary Common Lisp list of CSTs, each
   ;; representing a form of the body.
   (%form-csts :initarg :form-csts :accessor form-csts)
   (%block-name-cst :initform nil
                    :initarg :block-name-cst
                    :reader block-name-cst)))

(defun make-body (dspecs form-csts block-name-cst)
  (make-instance 'body
    :dspecs dspecs
    :form-csts form-csts
    :block-name-cst block-name-cst))

;;; Convert the body of a function.
(defun convert-body (body env system)
  (let ((new-env (augment-environment-with-declarations env (dspecs body)))
        (block-name-cst (block-name-cst body)))
    (convert (if block-name-cst
                 (cst:cstify (list* (cst:cst-from-expression 'block)
                                    block-name-cst
                                    (form-csts body)))

                 (cst:cstify (cons (cst:cst-from-expression 'block)
                                   (form-csts body))))
             new-env system)))

(defmethod process-parameter-groups
    ((parameter-groups null)
     idspecs
     body
     environment
     system)
  (values (convert-body body environment system) '()))

(defmethod process-parameter-groups
    ((parameter-groups cons)
     idspecs
     body
     environment
     system)
  (process-parameter-group (car parameter-groups)
                           (cdr parameter-groups)
                           idspecs
                           body
                           environment
                           system))

(defmethod process-parameter-group
    ((parameter-group cst:multi-parameter-group-mixin)
     remaining-parameter-groups
     idspecs
     body
     environment
     system)
  (process-parameters-in-group (cst:parameters parameter-group)
                               remaining-parameter-groups
                               idspecs
                               body
                               environment
                               system))

(defmethod process-parameter-group
    ((parameter-group cst:ordinary-rest-parameter-group)
     remaining-parameter-groups
     idspecs
     body
     environment
     system)
  (cons '&rest
        (process-parameter (cst:parameter parameter-group)
                           '()
                           remaining-parameter-groups
                           idspecs
                           body
                           environment
                           system)))

(defmethod process-parameter-group :around
    ((parameter-group cst:optional-parameter-group)
     remaining-parameter-groups
     idspecs
     body
     environment
     system)
  (cons '&optional (call-next-method)))

(defmethod process-parameter-group :around
    ((parameter-group cst:key-parameter-group)
     remaining-parameter-groups
     idspecs
     body
     environment
     system)
  (cons '&key
        (append (call-next-method)
                (if (cst:allow-other-keys parameter-group)
                    '(&allow-other-keys)
                    '()))))

(defmethod new-environment-from-parameter
    ((parameter cst:simple-variable) idspecs environment system)
  (augment-environment-with-variable (cst:raw (cst:name parameter))
                                     (first idspecs)
                                     environment
                                     environment))

(defmethod new-environment-from-parameter
    ((parameter cst:ordinary-key-parameter) idspecs environment system)
  (augment-environment-with-parameter (cst:raw (cst:name parameter))
                                      (cst:raw (cst:supplied-p parameter))
                                      (first idspecs)
                                      environment))

(defmethod new-environment-from-parameter
    ((parameter cst:ordinary-optional-parameter) idspecs environment system)
  (augment-environment-with-parameter (cst:raw (cst:name parameter))
                                      (cst:raw (cst:supplied-p parameter))
                                      (first idspecs)
                                      environment))

(defmethod process-parameter
    ((parameter cst:simple-variable)
     remaining-parameters-in-group
     remaining-parameter-groups
     idspecs
     body
     environment
     system)
  (let* ((var (cst:name parameter))
         (raw-var (cst:raw var))
         (origin (cst:source var))
         (name (make-symbol (string-downcase raw-var)))
         (lexical-ast (cleavir-ast:make-lexical-ast name :origin origin))
         (new-env (new-environment-from-parameter parameter
                                                  idspecs
                                                  environment
                                                  system)))
    (multiple-value-bind (ast lexical-lambda-list)
        (process-parameters-in-group remaining-parameters-in-group
                                     remaining-parameter-groups
                                     idspecs
                                     body
                                     environment
                                     system)
      (values (set-or-bind-variable
               var lexical-ast ast new-env system)
              (cons lexical-ast lexical-lambda-list)))))

(defmethod process-parameter
    ((parameter cst:ordinary-optional-parameter)
     remaining-parameters-in-group
     remaining-parameter-groups
     idspecs
     body
     environment
     system)
  (let* ((var-cst (cst:name parameter))
         (init-form-cst (cst:form parameter))
         (supplied-p-cst (cst:supplied-p parameter))
         (new-env (new-environment-from-parameter parameter
                                                  idspecs
                                                  environment
                                                  system))
         (init-ast (convert init-form-cst environment system)))
    (multiple-value-bind (ast lexical-lambda-list)
        (process-parameters-in-group remaining-parameters-in-group
                                     remaining-parameter-groups
                                     idspecs
                                     body
                                     new-env
                                     system)
      (multiple-value-bind (final-ast lexical-asts)
          (process-init-parameter var-cst
                                  supplied-p-cst
                                  init-ast
                                  new-env
                                  ast
                                  system)
        (values final-ast
                (cons lexical-asts lexical-lambda-list))))))

(defmethod process-parameter
    ((parameter cst:ordinary-key-parameter)
     remaining-parameters-in-group
     remaining-parameter-groups
     idspecs
     body
     environment
     system)
  (let* ((var-cst (cst:name parameter))
         (init-form-cst (cst:form parameter))
         (supplied-p-cst (cst:supplied-p parameter))
         (keyword-cst (cst:keyword parameter))
         (new-env (new-environment-from-parameter parameter
                                                  idspecs
                                                  environment
                                                  system))
         (init-ast (convert init-form-cst environment system)))
    (multiple-value-bind (ast lexical-lambda-list)
        (process-parameters-in-group remaining-parameters-in-group
                                     remaining-parameter-groups
                                     idspecs
                                     body
                                     new-env
                                     system)
      (multiple-value-bind (final-ast lexical-asts)
          (process-init-parameter var-cst
                                  supplied-p-cst
                                  init-ast
                                  new-env
                                  ast
                                  system)
        (values final-ast
                (cons (cons keyword-cst lexical-asts)
                      lexical-lambda-list))))))

(defmethod process-parameter
    ((parameter cst:aux-parameter)
     remaining-parameters-in-group
     remaining-parameter-groups
     idspecs
     body
     environment
     system)
  (let* ((var-cst (cst:name parameter))
         (init-form-cst (cst:form parameter))
         (new-env (new-environment-from-parameter parameter
                                                  idspecs
                                                  environment
                                                  system))
         (init-ast (convert init-form-cst environment system))
         (ast (process-parameters-in-group remaining-parameters-in-group
                                           remaining-parameter-groups
                                           idspecs
                                           body
                                           new-env
                                           system)))
    (values (set-or-bind-variable var-cst init-ast ast environment system)
            '())))

(defmethod convert-code (lambda-list-cst body-cst env system
			 &optional (block-name-cst nil))
  (let ((parsed-lambda-list
          (cst:parse-ordinary-lambda-list system lambda-list-cst)))
    (multiple-value-bind (declaration-csts documentation form-csts)
        (cst:separate-function-body body-cst)
      ;; FIXME: Handle documentation
      (declare (ignore documentation))
      (let* ((declaration-specifiers
               (loop for declaration-cst in declaration-csts
                     append (cdr (cst:listify declaration-cst))))
             (canonicalized-dspecs
               (cst:canonicalize-declaration-specifiers
                declaration-specifiers
                (cleavir-env:declarations env))))
	(multiple-value-bind (idspecs rdspecs)
	    (itemize-declaration-specifiers
	     (itemize-lambda-list parsed-lambda-list)
	     canonicalized-dspecs)
	  (multiple-value-bind (ast lexical-lambda-list)
              (process-parameter-groups
               (cst:children parsed-lambda-list)
               idspecs
               (make-body rdspecs form-csts block-name-cst)
               env
               system)
	    (cleavir-ast:make-function-ast ast lexical-lambda-list)))))))
