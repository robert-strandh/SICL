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

;;; When this variable is false, non-immediate constants will be
;;; converted into a LOAD-TIME-VALUE ast, which means that the machine
;;; code generated will be an access to an element in the vector of
;;; externals.  When it is true, such constants will instead be turned
;;; into code for creating them.
(defparameter *compile-for-linker* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting ordinary Common Lisp code.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a constant.
;;;
;;; Either the constant can be represented as an immediate value, or
;;; else it is turned into a LOAD-TIME-VALUE AST.

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
  (cond ((and (integerp constant)
	      (<= sicl-configuration:+most-negative-fixnum+
		  constant
		  sicl-configuration:+most-positive-fixnum+))
	 (sicl-ast:make-immediate-ast
	  (sicl-configuration:host-integer-to-word constant)))
	((characterp constant)
	 (sicl-ast:make-immediate-ast
	  (sicl-configuration:host-char-to-word constant)))
	(*compile-for-linker*
	 (convert-constant-for-linker constant))
	(t
	 ;; LOAD-TIME-VALUE takes a form, so if the form is
	 ;; a constant, it must be quoted. 
	 (sicl-ast:make-load-time-value-ast
	  `(quote ,constant)))))

(defun convert-variable (form env)
  (let ((info (sicl-env:variable-info form env)))
    (cond ((null info)
	   (error "undefined variable: ~s" form))
	  ((typep info 'sicl-env:constant-variable-info)
	   (convert-constant (sicl-env:definition info)))
	  (t
	   (sicl-env:location info)))))

(defgeneric convert-compound (head form environment))

(defun convert (form environment)
  (setf form (sicl-env:macroexpand form environment))
  (cond ((or (and (not (consp form))
		  (not (symbolp form)))
	     (and (symbolp form)
		  (or (keywordp form)
		      (member form '(t nil)))))
	 (convert-constant form))
	((and (consp form)
	      (eq (car form) 'quote))
	 (convert-constant (cadr form)))
	((symbolp form)
	 (convert-variable form environment))
	(t
	 (convert-compound (car form) form environment))))
	 
(defun convert-top-level-form (form)
  (convert `(function (lambda () ,form)) nil))

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
  (let ((info (sicl-env:function-info head env)))
    (if (null info)
	(error "no such function ~s" head)
	(sicl-ast:make-call-ast
	 (if (typep info 'sicl-env:global-location-info)
	     (sicl-ast:make-memref-ast
	      (sicl-ast:make-u+-ast
	       (list (sicl-ast:make-load-time-value-ast
		      `(sicl-word:find-function-cell ',head))
		     (sicl-ast:make-immediate-ast
		      sicl-configuration:+cdr-offset+))))
	     (sicl-env:location info))
	 (convert-sequence (cdr form) env)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convert an ordinary lambda list to compiled form.

;;; The main tricky part about converting a lambda list is that any
;;; init-form in &optional, &key, or &aux entries may refer to
;;; variables in parameter specifiers to the left of the one it
;;; appears in.
;;;
;;; A question (that the HyperSpec does not seem to be addressing as
;;; far as I can tell) is whether the lambda list may contain multiple
;;; entries of the same symbol.  It kind of makes sense that this
;;; would be possible, given what was said in the paragraph above, so
;;; a lambda list might be for instance: (x &key (x x)) which would
;;; mean that the variable x takes on the value of the :x keyword
;;; argument if given, and of the required arguement if no keyword
;;; argument was given.  It is probably easier to allow this than to
;;; check for it.

;;; FIXME: for now, we handle only required parameters.
(defun convert-lambda-list (ordinary-lambda-list env)
  (let* ((required-vars (sicl-code-utilities:required ordinary-lambda-list))
	 (new-env env))
    (loop for var in required-vars
	  do (setf new-env (sicl-env:add-lexical-variable-entry new-env var)))
    (values (make-instance 'sicl-ast:lambda-list
	      :required (loop for var in required-vars
			      for info = (sicl-env:variable-info var new-env)
			      for location = (sicl-env:location info)
			      collect location))
	    new-env)))

(defun convert-code (lambda-list body env)
  (let ((parsed-lambda-list
	  (sicl-code-utilities:parse-ordinary-lambda-list lambda-list)))
    (multiple-value-bind (compiled-lambda-list new-env)
	(convert-lambda-list parsed-lambda-list env)
      (multiple-value-bind (declarations documentation forms)
	  (sicl-code-utilities:separate-function-body body)
	(declare (ignore documentation))
	(setf new-env
	      (sicl-env:augment-environment-with-declarations
	       new-env declarations))
	(let ((body-asts (convert-sequence forms new-env)))
	  (sicl-ast:make-function-ast
	   compiled-lambda-list
	   (sicl-ast:make-progn-ast body-asts)))))))

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
  (let ((forms (convert-sequence (cddr form) environment)))
    (sicl-ast:make-eval-when-ast
     (cadr form)
     (sicl-ast:make-progn-ast forms))))

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
			   (sicl-env:location 
			    (sicl-env:function-info name new-env))
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
  (let ((info (sicl-env:function-info name environment)))
    (cond ((null info)
	   (error "no such function ~s" name))
	  ((typep info 'sicl-env:global-location-info)
	   (sicl-ast:make-memref-ast
	    (sicl-ast:make-u+-ast
	     (list (sicl-ast:make-load-time-value-ast
		    `(sicl-word:find-function-cell ',name))
		   (sicl-ast:make-immediate-ast
		    sicl-configuration:+cdr-offset+)))))
	  (t (sicl-env:location info)))))

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
;;; Converting LET.

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

;;; FIXME: We convert the LET to a function call with a lambda
;;; expression in the CAR, but this might not be quite right because a
;;; lambda expression may have documentation in it, whereas a LET may
;;; not.  This of course is a problem only when the LET form contains
;;; a string literal in a context where no value is required.  We may
;;; solve the problem by adding an empty comment in this case. 
;;;
;;; FIXME: If we are going to do it like this, we might as well turn
;;; LET into a macro, which is allowed by the HyperSpec.
(defmethod convert-compound
    ((symbol (eql 'let)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 nil)
  (destructuring-bind (bindings &rest body) (cdr form)
    (check-binding-forms bindings)
    (convert `((lambda ,(mapcar (lambda (v) (if (symbolp v) v (car v)))
			 bindings)
		 ,@body)
	       ,@(mapcar #'init-form bindings))
	     env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LET*.
;;;
;;; LET* can be converted to nested LETs, but one has to be careful to
;;; take apart the declarations and associate each one with the
;;; correct LET form.

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
      (sicl-ast:make-progn-ast
       (convert-sequence forms new-env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MACROLET.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting PROGN.

(defmethod convert-compound
    ((head (eql 'progn)) form environment)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-ast:make-progn-ast
   (convert-sequence (cdr form) environment)))

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
  (let ((info (sicl-env:variable-info var env)))
    (cond ((null info)
	   (error 'setq-unknown-variable
		  :form var))
	  ((typep info 'sicl-env:constant-variable-info)
	   (error 'setq-constant-variable
		  :form var))
	  ((or (typep info 'sicl-env:lexical-location-info)
	       (typep info 'sicl-env:special-location-info))
	   (sicl-ast:make-setq-ast
	    (sicl-env:location info)
	    (convert form env)))
	  ((typep info 'sicl-env:symbol-macro-info)
	   (convert `(setf ,(macroexpand var env) form) env))
	  (t
	   (error "this should not happen")))))
  
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
    (convert `(progn ,@(cddr form)) new-env)))

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
  (sicl-ast:make-the-ast
   (cadr form)
   (convert (caddr form) environment)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MEMALLOC.

(defmethod convert-compound
    ((symbol (eql 'sicl-word:memalloc)) form environment)
  (sicl-ast:make-call-ast
   (sicl-ast:make-memref-ast 
    (sicl-ast:make-immediate-ast (+ (ash 1 30) 0)))
   (convert-arguments (cdr form) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FIND-PACKAGE.

(defmethod convert-compound
    ((symbol (eql 'find-package)) form environment)
  (sicl-ast:make-call-ast
   (sicl-ast:make-memref-ast 
    (sicl-ast:make-immediate-ast (+ (ash 1 30) 4)))
   (convert-arguments (cdr form) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FIND-SYMBOL.

(defmethod convert-compound
    ((symbol (eql 'find-symbol)) form environment)
  (sicl-ast:make-call-ast
   (sicl-ast:make-memref-ast 
    (sicl-ast:make-immediate-ast (+ (ash 1 30) 8)))
   (convert-arguments (cdr form) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FIND-FUNCTION-CELL.

(defmethod convert-compound
    ((symbol (eql 'sicl-word:find-function-cell)) form environment)
  (sicl-ast:make-call-ast
   (sicl-ast:make-memref-ast 
    (sicl-ast:make-immediate-ast (+ (ash 1 30) 12)))
   (convert-arguments (cdr form) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FIND-CLASS.

(defmethod convert-compound
    ((symbol (eql 'find-class)) form environment)
  (sicl-ast:make-call-ast
   (sicl-ast:make-memref-ast 
    (sicl-ast:make-immediate-ast (+ (ash 1 30) 16)))
   (convert-arguments (cdr form) environment)))

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
  (sicl-ast:make-immediate-ast (cadr form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MEMREF.

(defmethod convert-compound ((symbol (eql 'sicl-word:memref)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 1)
  (sicl-ast:make-memref-ast (car (convert-arguments (cdr form) env))))

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
;;; Converting HALT.

(defmethod convert-compound ((symbol (eql 'sicl-word:halt)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 0 0)
  (sicl-ast:make-halt-ast))
