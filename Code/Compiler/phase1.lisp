(in-package #:sicl-compiler-phase-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The base classes for conditions used here. 

(define-condition compilation-program-error (program-error)
  ((%expr :initarg :expr :reader expr)))

(define-condition compilation-warning (warning)
  ((%expr :initarg :expr :reader expr)))

(define-condition compilation-style-warning (style-warning)
  ((%expr :initarg :expr :reader expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting code to an abstract syntax tree.

;;; When this variable is false, we are invoked by EVAL or COMPILE.
;;; When it is true, we are invoked by COMPILE-FILE.
(defparameter *compile-file* nil)

;;; This variable holds the processing mode for top-level forms
;;; processed by COMPILE-FILE.
(defparameter *compile-time-too* nil)

;;; This variable is TRUE as long as a form is considered to be a
;;; top-level form.  It is bound to the value of *TOP-LEVEL-SUBFORM-P*
;;; before a subform is processed.
(defparameter *top-level-form-p* t)

;;; The value of this variable is normally FALSE.  It is bound to the
;;; value of *TOP-LEVEL-FORM-P* by certain converters that need to
;;; process subforms the same way as the form itself.
(defparameter *top-level-subform-p* t)

;;; When this variable is false, non-immediate constants will be
;;; converted into a LOAD-TIME-VALUE ast, which means that the machine
;;; code generated will be an access to an element in the vector of
;;; externals.  When it is true, such constants will instead be turned
;;; into code for creating them.
(defparameter *compile-for-linker* nil)

;;; During conversion, this variable contains a hash table that maps
;;; from instance of environment locations to ASTs.
(defparameter *location-asts* nil)

(defun find-or-create-ast (location)
  (or (gethash location *location-asts*)
      (let ((ast (etypecase location
		   (sicl-env:lexical-location
		    (sicl-ast:make-lexical-ast
		     (sicl-env:name location)))
		   (sicl-env:global-location
		    (sicl-ast:make-global-ast
		     (sicl-env:name location)
		     (sicl-env:storage location)))
		   (sicl-env:special-location
		    (sicl-ast:make-special-ast
		     (sicl-env:name location)
		     (sicl-env:storage location))))))
	(setf (gethash location *location-asts*) ast))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting ordinary Common Lisp code.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a constant.
;;;

(defun convert-string (string)
  (let ((contents-var (gensym))
	(string-var (gensym)))
    (convert
     `(let ((,contents-var
	      (sicl-word:memalloc
	       (sicl-word:word ,(* 4 (1+ (length string)))))))
	(sicl-word:memset ,contents-var ,(length string))
	,@(loop for char across string
		for i from 4 by 4
		collect `(sicl-word:memset
			  (sicl-word:u+ ,contents-var
					(sicl-word:word ,i))
			  ,char))
	(let ((,string-var
		(sicl-word:memalloc (sicl-word:word 8))))
	  (sicl-word:memset
	   ,string-var
	   (sicl-word:memref
	    (sicl-word:word ,(+ (ash 1 30) 20))))
	  (sicl-word:memset
	   (sicl-word:u+ ,string-var (sicl-word:word 4))
	   ,contents-var)
	  (sicl-word:u+ ,string-var (sicl-word:word 3))))
     nil)))

;;; This function is only called for constants that can not
;;; be represented as immediates. 
(defun convert-constant-for-linker (constant)
  (cond ((stringp constant)
	 (convert-string constant))
	((symbolp constant)
	 (convert `(find-symbol
		    ,(symbol-name constant)
		    (find-package
		     ,(package-name (symbol-package constant))))
		  nil))))	 

;;; When the constant is quoted, this function is called with the 
;;; surrounding QUOTE form stripped off. 
(defun convert-constant (constant)
  (sicl-ast:make-constant-ast constant))

(defun convert-variable (form env)
  (let ((info (sicl-env:variable-info form env t)))
    (typecase info
      (sicl-env:constant-variable-info
       (convert-constant (sicl-env:definition info)))
      (t
       (find-or-create-ast (sicl-env:location info))))))

(defgeneric convert-compound (head form environment))

(defgeneric convert (form environment))

(defun convert-initial (form)
  (let ((*location-asts* (make-hash-table :test #'eq)))
    (convert form nil)))

(defun convert-top-level-form (form)
  (let ((*location-asts* (make-hash-table :test #'eq)))
    (convert `(function (lambda () ,form)) nil)))

(defun convert-top-level-lamda-expression (lambda-expression)
  (unless (and (consp lambda-expression)
	       (eq (car lambda-expression) 'lambda))
    (error "argument must be a lambda expression"))
  (let ((*location-asts* (make-hash-table :test #'eq)))
    (convert `(function ,lambda-expression) nil)))

(defun convert-for-inlining (lambda-expression)
  (let* ((lambda-list (cadr lambda-expression))
	 (let-bindings (loop for var in lambda-list
			     for i from 0
			     collect `(,var (arg ,i))))
	 (*location-asts* (make-hash-table :test #'eq)))
    (let ((ast (convert `(let ,let-bindings ,@(cddr lambda-expression)) nil)))
      ;; The AST looks like this:
      ;; (progn (setq <a0> (arg 0)) (progn (setq <a1> (arg 1)) ....
      (loop for arg in lambda-list
	    collect (first (sicl-ast:children (first (sicl-ast:children ast))))
	      into lexical-asts
	    do (setf ast (second (sicl-ast:children ast)))
	    finally (return (values lexical-asts ast))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a sequence of forms.

(defun convert-sequence (forms environment)
  (loop for form in forms
	collect (convert form environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a function call.

;;; Default method when there is not a more specific method for
;;; the head symbol.
(defmethod convert-compound ((head symbol) form env)
  (let ((info (sicl-env:function-info head env t))
	(arguments (convert-sequence (cdr form) env)))
    (if (and (eq (sicl-env:inline-info info) :inline)
	     (not (null (sicl-env:ast info)))
	     (= (length (cdr form)) (length (sicl-env:parameters info))))
	(sicl-ast:make-progn-ast
	 (append (loop for parameter in (sicl-env:parameters info)
		       for argument in arguments
		       collect (sicl-ast:make-setq-ast parameter argument))
		 (list (sicl-env:ast info))))
	(let* ((global-ast (find-or-create-ast (sicl-env:location info)))
	       (ast (sicl-ast:make-call-ast global-ast arguments)))
	  (setf (sicl-ast:function-type global-ast)
		(sicl-env:type info))
	  ast))))

;;; Method to be used when the head of a compound form is a
;;; CONS.  Then the head must be a lambda expression.
;;; We replace a call such as ((lambda (params) . body) . args)
;;; by (flet ((temp (params) . body)) (temp . args))
;;;
;;; FIXME: do some more error checking.
(defmethod convert-compound ((head cons) form env)
  (destructuring-bind ((lambda lambda-list &rest body) &rest args) form
    (declare (ignore lambda))
    (sicl-ast:make-call-ast
     (convert-code lambda-list body env)
     (convert-sequence args env))))


(defun convert-code (lambda-list body env)
  (let ((parsed-lambda-list
	  (sicl-code-utilities:parse-ordinary-lambda-list lambda-list)))
    (sicl-ast:make-function-ast
     (convert `(let* ,(sicl-code-utilities:match-lambda-list
		       parsed-lambda-list '(argcount) 'arg)
		 ,@body)
	      env)
     (if (and (eq (sicl-code-utilities:optionals parsed-lambda-list) :none)
	      (eq (sicl-code-utilities:rest-body parsed-lambda-list) :none)
	      (eq (sicl-code-utilities:keys parsed-lambda-list) :none))
	 (length (sicl-code-utilities:required parsed-lambda-list))
	 nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting BLOCK.

(define-condition block-name-must-be-a-symbol
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'block)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 nil)
  (unless (symbolp (cadr form))
    (error 'block-name-must-be-a-symbol
	   :expr (cadr form)))
  (let* ((ast (sicl-ast:make-block-ast nil))
	 (new-env (sicl-env:add-block-entry env (cadr form) ast))
	 (forms (convert-sequence (cddr form) new-env)))
    (setf (sicl-ast:body-ast ast)
	  (sicl-ast:make-progn-ast forms))
    ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting EVAL-WHEN.

(define-condition situations-must-be-proper-list
    (compilation-program-error)
  ())

(define-condition invalid-eval-when-situation
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'eval-when)) form environment)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 nil)
  (unless (sicl-code-utilities:proper-list-p (cadr form))
    (error 'situations-must-be-proper-list
	   :expr (cadr form)))
  ;; Check each situation
  (loop for situation in (cadr form)
	do (unless (and (symbolp situation)
			(member situation
				'(:compile-toplevel :load-toplevel :execute
				  compile load eval)))
	     ;; FIXME: perhaps we should warn about the deprecated situations
	     (error 'invalid-eval-when-situation
		    :expr situation)))
  (let* ((situations (cadr form))
	 (c (not (null (intersection '(:compile-toplevel compile) situations))))
	 (l (not (null (intersection '(:load-toplevel load) situations))))
	 (e (not (null (intersection '(:execute eval) situations)))))
    (let ((new-form (cons 'progn (cddr form))))
      (if (and *compile-file* *top-level-form-p*)
	  (let ((*top-level-subform-p* *top-level-form-p*))
	    ;; This test tree corresponds to figure 3-7 of the
	    ;; HyperSpec.
	    (if c
		(if l
		    (let ((*compile-time-too* t))
		      (convert new-form environment))
		    (progn (eval new-form)
			   (convert ''nil environment)))
		(if l
		    (if e
			(convert new-form environment)
			(let ((*compile-time-too* nil))
			  (convert new-form environment)))
		    (if (and e *compile-time-too*)
			(progn (eval new-form)
			       (convert ''nil environment))
			(convert ''nil environment)))))
	  (if e
	      (convert new-form environment)
	      (convert ''nil environment))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FLET.

(define-condition flet-functions-must-be-proper-list
    (compilation-program-error)
  ())

(define-condition functions-body-must-be-proper-list
    (compilation-program-error)
  ())

(defmethod convert-compound ((symbol (eql 'flet)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 nil)
  (unless (sicl-code-utilities:proper-list-p (cadr form))
    (error 'flet-functions-must-be-proper-list
	   :expr form))
  (let ((new-env env))
    ;; Create a new environment with the additional names.
    (loop for def in (cadr form)
	  for name = (car def)
	  do (setf new-env (sicl-env:add-local-function-entry new-env name)))
    (let ((init-asts
	    (loop for (name lambda-list . body) in (cadr form)
		  for fun = (convert-code lambda-list body env)
		  collect (sicl-ast:make-setq-ast
			   (let* ((info (sicl-env:function-info name new-env t))
				  (location (sicl-env:location info)))
			     (find-or-create-ast location))
			   fun))))
      (multiple-value-bind (declarations forms)
	  (sicl-code-utilities:separate-ordinary-body (cddr form))
	(setf new-env (sicl-env:augment-environment-with-declarations
		       new-env declarations))
	(sicl-ast:make-progn-ast
	 (append init-asts (convert-sequence forms new-env)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FUNCTION.

(define-condition lambda-must-be-proper-list
    (compilation-program-error)
  ())

(define-condition function-argument-must-be-function-name-or-lambda-expression
    (compilation-program-error)
  ())

(defun convert-named-function (name environment)
  (let* ((info (sicl-env:function-info name environment t))
	 (location (sicl-env:location info)))
    (find-or-create-ast location)))

(defun convert-lambda-function (lambda-form env)
  (unless (sicl-code-utilities:proper-list-p lambda-form)
    (error 'lambda-must-be-proper-list
	   :expr lambda-form))
  (convert-code (cadr lambda-form) (cddr lambda-form) env))

(defun proper-function-name-p (name)
  (or (symbolp name)
      (and (sicl-code-utilities:proper-list-p name)
	   (= (length name) 2)
	   (eq (car name) 'setf)
	   (symbolp (cadr name)))))

(defmethod convert-compound ((symbol (eql 'function)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 1)
  (unless (or (proper-function-name-p (cadr form))
	      (and (consp (cadr form))
		   (eq (car (cadr form)) 'lambda)))
    (error 'function-argument-must-be-function-name-or-lambda-expression
	   :expr (cadr form)))
  (if (proper-function-name-p (cadr form))
      (convert-named-function (cadr form) env)
      (convert-lambda-function (cadr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting GO.

(defmethod convert-compound ((symbol (eql 'go)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 1)
  (let ((info (sicl-env:tag-info (cadr form) env)))
    (if (null info)
	(error "undefined go tag: ~s" form)
	(sicl-ast:make-go-ast
	 (sicl-env:definition info)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting IF.

(defmethod convert-compound ((symbol (eql 'if)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 3)
  (sicl-ast:make-if-ast
   (convert (cadr form) env)
   (convert (caddr form) env)
   (if (null (cdddr form))
       (convert-constant nil)
       (convert (cadddr form) env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LABELS.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LET and LET*

(define-condition bindings-must-be-proper-list
    (compilation-program-error)
  ())

(define-condition binding-must-be-symbol-or-list
    (compilation-program-error)
  ())

(define-condition binding-must-have-length-one-or-two
    (compilation-program-error)
  ())

(define-condition variable-must-be-a-symbol
    (compilation-program-error)
  ())    

(defun check-binding-forms (binding-forms)
  (unless (sicl-code-utilities:proper-list-p binding-forms)
    (error 'bindings-must-be-proper-list :expr binding-forms))
  (loop for binding-form in binding-forms
	do (unless (or (symbolp binding-form) (consp binding-form))
	     (error 'binding-must-be-symbol-or-list
		    :expr binding-form))
	   (when (and (consp binding-form)
		      (or (not (listp (cdr binding-form)))
			  (not (null (cddr binding-form)))))
	     (error 'binding-must-have-length-one-or-two
		    :expr binding-form))
	   (when (and (consp binding-form)
		      (not (symbolp (car binding-form))))
	     (error 'variable-must-be-a-symbol
		    :expr (car binding-form)))))

(defun init-form (binding)
  (if (or (symbolp binding) (null (cdr binding)))
      nil
      (cadr binding)))

(defun convert-simple-let (binding body env)
  (let* ((var (if (symbolp binding) binding (car binding)))
	 (init-form (if (symbolp binding) nil (cadr binding)))
	 (new-env (sicl-env:add-lexical-variable-entry env var))
	 (info (sicl-env:variable-info var new-env))
	 (location (sicl-env:location info)))
    (multiple-value-bind (declarations forms)
	(sicl-code-utilities:separate-ordinary-body body)
      ;; FIXME: handle declarations
      ;; FIXME: in particular, if there is a SPECIAL declaration
      ;; then generate totally different code. 
      (declare (ignore declarations))
      (sicl-ast:make-progn-ast
       (cons (sicl-ast:make-setq-ast
	      (find-or-create-ast location)
	      (convert init-form env))
	     (convert-sequence forms new-env))))))

;;; Separate a list of canonicalized declaration specifiers into two
;;; disjoint sets, returned as two values.  The first set contains All
;;; the declerations specifiers that concern an ordinary variable
;;; named NAME, and the second set the remaining declaration specifiers.
(defun separate-declarations (canonicalized-declaration-specifiers name)
  (loop for spec in canonicalized-declaration-specifiers
	if (or (and (eq (first spec) 'ignore)
		    (eq (second spec) name))
	       (and (eq (first spec) 'ignorable)
		    (eq (second spec) name))
	       (and (eq (first spec) 'dynamic-extent)
		    (eq (second spec) name))
	       (and (eq (first spec) 'special)
		    (eq (second spec) name))
	       (and (eq (first spec) 'type)
		    (eq (third spec) name)))
	  collect spec into first
	else
	  collect spec into second
	finally (return (values first second))))

;;; We convert a LET form recursively.  If it has a single binding, we
;;; convert it into a SETQ.  If it has more than one binding, we
;;; convert it as follows:
;;;
;;; (let ((<var> <init-form>)
;;;       <more-bindings>)
;;;   <body>)
;;; =>
;;; (let ((temp <init-form>))
;;;   (let (<more-bindings>)
;;;     (let ((<var> temp))
;;;       <body>)))

(defmethod convert-compound
    ((symbol (eql 'let)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 nil)
  (destructuring-bind (bindings &rest body) (cdr form)
    (check-binding-forms bindings)
    (if (= (length bindings) 1)
	(convert-simple-let (car bindings) body env)
	(let* ((first (car bindings))
	       (var (if (symbolp first) first (car first)))
	       (init-form (if (symbolp first) nil (cadr first)))
	       (temp (gensym)))
	  (multiple-value-bind (declarations forms)
	      (sicl-code-utilities:separate-ordinary-body body)
	    (multiple-value-bind (first remaining)
		(separate-declarations 
		 (sicl-code-utilities:canonicalize-declaration-specifiers 
		  (mapcar #'cdr declarations))
		 var)
	      (convert
	       `(let ((,temp ,init-form))
		  (let ,(cdr bindings)
		    (declare ,@remaining)
		    (let ((,var ,temp))
		      (declare ,@first)
		      ,@forms)))
	       env)))))))

;;; We convert a LET* form recursively.  If it has a single binding,
;;; we convert it into a SETQ.  If it has more than one binding, we
;;; convert it as follows:
;;;
;;; (let* ((<var> <init-form>)
;;;        <more-bindings>)
;;;   <body>)
;;; =>
;;; (let ((<var> <init-form>))
;;;   (let (<more-bindings>)
;;;      <body>)))

(defmethod convert-compound
    ((symbol (eql 'let*)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 nil)
  (destructuring-bind (bindings &rest body) (cdr form)
    (check-binding-forms bindings)
    (if (= (length bindings) 1)
	(convert-simple-let (car bindings) body env)
	(let* ((first (car bindings))
	       (var (if (symbolp first) first (car first)))
	       (init-form (if (symbolp first) nil (cadr first))))
	  (multiple-value-bind (declarations forms)
	      (sicl-code-utilities:separate-ordinary-body body)
	    (multiple-value-bind (first remaining)
		(separate-declarations 
		 (sicl-code-utilities:canonicalize-declaration-specifiers 
		  (mapcar #'cdr declarations))
		 var)
	      (convert
	       `(let ((,var ,init-form))
		  (declare ,@first)
		  (let* ,(cdr bindings)
		    (declare ,@remaining)
		    ,@forms))
	       env)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LOAD-TIME-VALUE.

(define-condition load-time-value-must-have-one-or-two-arguments
    (compilation-program-error)
  ())

(define-condition read-only-p-must-be-boolean
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'load-time-value)) form environment)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 2)
  (unless (null (cddr form))
    ;; The HyperSpec specifically requires a "boolean"
    ;; and not a "generalized boolean".
    (unless (member (caddr form) '(nil t))
      (error 'read-only-p-must-be-boolean
	     :expr (caddr form))))
  (sicl-ast:make-load-time-value-ast
   (convert (cadr form) environment)
   (caddr form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LOCALLY.

(defmethod convert-compound
    ((symbol (eql 'locally)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (multiple-value-bind (declarations forms)
      (sicl-code-utilities:separate-ordinary-body (cdr form))
    (let ((new-env (sicl-env:augment-environment-with-declarations
		    env declarations)))
      ;; According to section 3.2.3.1 of the HyperSpec, LOCALLY
      ;; processes its subforms the same way as the form itself.
      (let ((*top-level-subform-p* *top-level-form-p*))
	(sicl-ast:make-progn-ast
	 (convert-sequence forms new-env))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MACROLET.

;; According to section 3.2.3.1 of the HyperSpec, MACROLET processes
;; its subforms the same way as the form itself.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting PROGN.

(defmethod convert-compound
    ((head (eql 'progn)) form environment)
  (sicl-code-utilities:check-form-proper-list form)
  ;; According to section 3.2.3.1 of the HyperSpec, PROGN
  ;; processes its subforms the same way as the form itself.
  (let ((*top-level-subform-p* *top-level-form-p*))
    (sicl-ast:make-progn-ast
     (convert-sequence (cdr form) environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting RETURN-FROM.

(define-condition block-name-unknown
    (compilation-program-error)
  ())

(defmethod convert-compound ((symbol (eql 'return-from)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 2)
  (unless (symbolp (cadr form))
    (error 'block-name-must-be-a-symbol
	   :expr (cadr form)))
  (let ((info (sicl-env:block-info (cadr form) env)))
    (if (null info)
	(error 'block-name-unknown
	       :expr (cadr form))
	(sicl-ast:make-return-from-ast
	 (sicl-env:definition info)
	 (convert (caddr form) env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SETQ.

(define-condition setq-must-have-even-number-of-arguments
    (compilation-program-error)
  ())

(define-condition setq-var-must-be-symbol
    (compilation-program-error)
  ())

(define-condition setq-unknown-variable
    (compilation-program-error)
  ())

(define-condition setq-constant-variable
    (compilation-program-error)
  ())

(defun convert-elementary-setq (var form env)
  (unless (symbolp var)
    (error 'setq-var-must-be-symbol
	   :expr var))
  (let* ((info (sicl-env:variable-info var env t))
	 (location (sicl-env:location info)))
    (if (typep info 'sicl-env:constant-variable-info)
	(error 'setq-constant-variable
	       :form var)
	(sicl-ast:make-setq-ast
	 (find-or-create-ast location)
	 (convert form env)))))
  
(defmethod convert-compound
    ((symbol (eql 'setq)) form environment)
  (sicl-code-utilities:check-form-proper-list form)
  (unless (oddp (length form))
    (error 'setq-must-have-even-number-of-arguments
	   :expr form))
  (let ((form-asts (loop for (var form) on (cdr form) by #'cddr
			 collect (convert-elementary-setq
				  var form environment))))
    (sicl-ast:make-progn-ast form-asts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SYMBOL-MACROLET.

(defmethod convert-compound
    ((head (eql 'symbol-macrolet)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 nil)
  ;; FIXME: syntax check bindings
  (let ((new-env env))
    (loop for (name expansion) in (cadr form)
	  do (setf new-env
		   (sicl-env:add-symbol-macro-entry new-env name expansion)))
    ;; According to section 3.2.3.1 of the HyperSpec, SYMBOL-MACROLET
    ;; processes its subforms the same way as the form itself.
    (let ((*top-level-subform-p* *top-level-form-p*))
      (convert `(progn ,@(cddr form)) new-env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting TAGBODY.

(defmethod convert-compound
    ((symbol (eql 'tagbody)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (let ((tag-asts
	  (loop for item in (cdr form)
		when (symbolp item)
		  collect (sicl-ast:make-tag-ast item)))
	(new-env env))
    (loop for ast in tag-asts
	  do (setf new-env (sicl-env:add-go-tag-entry
			    new-env (sicl-ast:name ast) ast)))
    (let ((items (loop for item in (cdr form)
		       collect (if (symbolp item)
				   (pop tag-asts)
				   (convert item new-env)))))
      (sicl-ast:make-tagbody-ast items))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting THE.

(defmethod convert-compound
    ((symbol (eql 'the)) form environment)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (apply #'sicl-ast:make-the-ast
	 (convert (caddr form) environment)
	 (mapcar #'convert-constant
		 (if (and (consp (cadr form)) (eq (car (cadr form)) 'values))
		     (cdr (cadr form))
		     (list (cadr form))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FUNCALL.
;;;
;;; While FUNCALL is a function and not a special operator, we convert
;;; it here anyway.

(defmethod convert-compound
    ((symbol (eql 'funcall)) form environment)
  (let ((args (convert-sequence (cdr form) environment)))
    (sicl-ast:make-call-ast (car args) (cdr args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting code for low-level operators.

(defun convert-arguments (arguments env)
  (loop for argument in arguments
	collect (convert argument env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting WORD.

(defmethod convert-compound ((symbol (eql 'sicl-word:word)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 1)
  (sicl-ast:make-word-ast (cadr form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MEMREF.

(defmethod convert-compound ((symbol (eql 'sicl-word:memref)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (sicl-ast:make-memref-ast
   (car (convert-arguments (list (cadr form)) env))
   (caddr form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MEMSET.

(defmethod convert-compound ((symbol (eql 'sicl-word:memset)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (sicl-ast:make-memset-ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting U+.

(defmethod convert-compound ((symbol (eql 'sicl-word:u+)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 3)
  (sicl-ast:make-u+-ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting U-.

(defmethod convert-compound ((symbol (eql 'sicl-word:u-)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 3)
  (sicl-ast:make-u--ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting S+.

(defmethod convert-compound ((symbol (eql 'sicl-word:s+)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (sicl-ast:make-s+-ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting S-.

(defmethod convert-compound ((symbol (eql 'sicl-word:s-)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (sicl-ast:make-s--ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting NEG.

(defmethod convert-compound ((symbol (eql 'sicl-word:neg)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 1)
  (sicl-ast:make-neg-ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting &.

(defmethod convert-compound ((symbol (eql 'sicl-word:&)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (sicl-ast:make-&-ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting IOR.

(defmethod convert-compound ((symbol (eql 'sicl-word:ior)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (sicl-ast:make-ior-ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting XOR.

(defmethod convert-compound ((symbol (eql 'sicl-word:xor)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (sicl-ast:make-xor-ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting ~.

(defmethod convert-compound ((symbol (eql 'sicl-word:~)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (sicl-ast:make-~-ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting ==.

(defmethod convert-compound ((symbol (eql 'sicl-word:==)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (sicl-ast:make-==-ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting S<.

(defmethod convert-compound ((symbol (eql 'sicl-word:s<)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (sicl-ast:make-s<-ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting S<=.

(defmethod convert-compound ((symbol (eql 'sicl-word:s<=)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (sicl-ast:make-s<=-ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting U<.

(defmethod convert-compound ((symbol (eql 'sicl-word:u<)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (sicl-ast:make-u<-ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting U<=.

(defmethod convert-compound ((symbol (eql 'sicl-word:u<=)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (sicl-ast:make-u<=-ast (convert-arguments (cdr form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting ARGCOUNT.

(defmethod convert-compound ((symbol (eql 'argcount)) form env)
  (sicl-ast:make-argcount-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting ARG.

(defmethod convert-compound ((symbol (eql 'arg)) form env)
  (sicl-ast:make-arg-ast
   (convert (second form) env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting TYPEQ.

(defmethod convert-compound ((symbol (eql 'sicl-type:typeq)) form env)
  (sicl-ast:make-typeq-ast
   (convert (second form) env)
   (convert-constant (third form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting HALT.

(defmethod convert-compound ((symbol (eql 'sicl-word:halt)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 0 0)
  (sicl-ast:make-halt-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONVERT is the function that must be called by every conversion
;;; attempt.  
;;;
;;; This function has an unusual control structure.  The reason is the
;;; complication that arises because of the way top-level forms are
;;; handled.  Whether a form is a top-level form or not is indicated
;;; by the variable *TOP-LEVEL-FORM-P*.  This variable is initially
;;; TRUE, and we want it to become FALSE by default whenever CONVERT
;;; is called recursively.  In some cases, however, subforms of some
;;; form F should be processed the same way as F is processed (see
;;; section 3.2.3.1 in the HyperSpec).  The simplest control structure
;;; would be for all indivudual converters to set *TOP-LEVEL-FORM-P*
;;; to FALSE, EXCEPT those that process subforms the same way.
;;; However, that would require us to take special action whenever we
;;; add a converter for a new special form, and we would like to avoid
;;; that.  We would like the special action to be visible only in the
;;; converters that treat subforms as top-level forms.
;;;
;;; The way we solve this problem is as follows: We use a second
;;; variable *TOP-LEVEL-SUBFORM-P*, which is normally FALSE.  An
;;; :AROUND method on CONVERT binds *TOP-LEVEL-FORM-P* to the value of
;;; *TOP-LEVEL-SUBFORM-P*, and binds *TOP-LEVEL-SUBFORM-P* to FALSE.
;;; Individual converters that need to process subforms the same way
;;; bind *TOP-LEVEL-SUBFORM-P* to the value of *TOP-LEVEL-FORM-P*
;;; before calling CONVERT.


(defmethod convert (form environment)
  (setf form (sicl-env:fully-expand-form form environment))
  (cond ((and (not (consp form))
	      (not (symbolp form)))
	 (convert-constant form))
	((and (symbolp form)
	      (sicl-env:constantp form environment))
	 (convert-constant (sicl-env:symbol-value form)))
	((and (consp form)
	      (eq (car form) 'quote))
	 (convert-constant (cadr form)))
	((symbolp form)
	 (convert-variable form environment))
	(t
	 (convert-compound (car form) form environment))))
	 
(defmethod convert :around (form environment)
  (let ((*top-level-form-p* *top-level-subform-p*)
	(*top-level-subform-p* nil))
    (when (and *compile-time-too* *top-level-form-p*)
      (eval form))
    (call-next-method)))
