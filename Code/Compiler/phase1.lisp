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

(defclass ast () ())

(defclass constant-ast (ast)
  ((%value :initarg :value :reader value)))

(defclass typed-location-ast (ast)
  ((%location :initarg :location :reader location)
   (%type :initarg :type :reader type)))

(defun convert-constant (form)
  (make-instance 'constant-ast :value form))

(defun convert-variable (form env)
  (let ((entry (sicl-env:find-variable form env)))
    (cond ((null entry)
	   (warn "undefined variable: ~s" form))
	  ((typep entry 'sicl-env:constant-variable-entry)
	   (make-instance 'constant-ast
	     :value (sicl-env:definition entry)))
	  (t
	   (let* ((location (sicl-env:location entry))
		  (type (sicl-env:find-type location env)))
	     (make-instance 'typed-location-ast
			    :location location 
			    :type type))))))

(defgeneric convert-compound (head form environment))

(defun convert (form environment)
  (setf form (sicl-env:macroexpand form environment))
  (cond ((or (and (not (consp form))
		  (not (symbolp form)))
	     (and (symbolp form)
		  (or (keywordp form)
		      (member form '(t nil))))
	     (and (consp form)
		  (eq (car form) 'quote)))
	 (convert-constant form))
	((symbolp form)
	 (convert-variable form environment))
	(t
	 (convert-compound (car form) form environment))))
	 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a sequence of forms.

(defun convert-sequence (forms environment)
  (loop for form in forms
	collect (convert form environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a function call.

(defclass function-call-ast (ast)
  ((%function-location :initarg :function-location :reader function-location)
   (%type :initarg :type :reader type)
   (%arguments :initarg :arguments :reader arguments)))

;;; Default method when there is not a more specific method for
;;; the head symbol.
(defmethod convert-compound ((head symbol) form env)
  (let* ((entry (sicl-env:find-function head env)))
    (if (null entry)
	(warn "no such function")
	(let* ((location (sicl-env:location entry))
	       (type (sicl-env:find-ftype location env)))
	  (make-instance 'function-call-ast
	    :function-location location
	    :type type
	    :arguments (convert-sequence (cdr form) env))))))

;;; Method to be used when the head of a compound form is a
;;; CONS.  Then the head must be a lambda expression.
;;; We replace a call such as ((lambda (params) . body) . args)
;;; by (flet ((temp (params) . body)) (temp . args))
;;;
;;; FIXME: do some more error checking.
(defmethod convert-compound ((head cons) form env)
  (destructuring-bind ((lambda params &rest body) &rest args) form
    (declare (ignore lambda))
    (let ((temp (gensym)))
      (convert `(flet ((,temp ,params ,@body))
		  (,temp ,@args))
	       env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convert an ordinary lambda list to compiled form.

;;; An &optional entry is one of:
;;;
;;;  * (lexical-location init-form-ast)
;;;  * (lexical-location init-form-ast lexical-location)
;;;
;;; Here, init-form-ast is an AST resulting from the conversion of an
;;; initialization form.  In the second variant, the last lexical
;;; location is assigned a Boolean value according to whether an
;;; optional argument was given.
;;;
;;; A &key entry is one of:
;;;
;;;  * ((keyword lexical-location) init-form-ast)
;;;  * ((keyword lexical-location) init-form-ast lexical-location)
;;;
;;; As with the &optional entry, init-form-ast is an AST resulting
;;; from the conversion of an initialization form.  In the second
;;; variant, the last lexical location is assigned a Boolean value
;;; according to whether an optional argument was given.
;;; 
;;; An &aux entry is of the form:
;;;
;;;  * (lexical-location init-form-ast)

;;; FIXME: handle special variables in lambda list. 

(defclass lambda-list ()
  (;; A possibly empty list of lexical locations.
   (%required :initarg :required :reader required)
   ;; A possibly empty list of optional entries. 
   (%optional :initarg :optional :reader optional)
   ;; Either NIL or a single lexical location. 
   (%rest-body :initarg :rest-body :reader rest-body)
   ;; Either:
   ;;  * :none, meaning &key was not given at all,
   ;;  * a possibly empty list of &key entries.
   (%keys :initarg :keys :reader keys)
   ;; Either:
   ;;  * nil, meaning &allow-other-keys was not given at all,
   ;;  * t, meaning &allow-other-keys was given.
   (%allow-other-keys :initarg :allow-other-keys :reader allow-other-keys)
   ;; A possibly empty list of &aux entries.
   (%aux :initarg :aux :reader aux)))

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
	 (required-entries
	   (loop for var in required-vars
		 collect (sicl-env:make-lexical-variable-entry var)))
	 (new-env (sicl-env:augment-environment env required-entries)))
    (values
     (make-instance 'lambda-list
		    :required (mapcar #'sicl-env:location required-entries))
     new-env)))

(defclass function-ast (ast)
  ((%lambda-list :initarg :lambda-list :reader lambda-list)
   (%body-ast :initarg :body-ast :reader body-ast)))

(defun convert-code (lambda-list body env)
  (let* ((parsed-lambda-list
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
	  (make-instance 'function-ast
			 :lambda-list compiled-lambda-list
			 :body-ast (make-instance 'progn-ast
						  :form-asts body-asts)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting BLOCK.

(defclass block-ast (ast)
  ((%body :initarg :body :accessor body)))

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
  (let* ((ast (make-instance 'block-ast))
	 (entry (sicl-env:make-block-entry (cadr form) ast))
	 (new-env (sicl-env:augment-environment env (list entry)))
	 (forms (convert-sequence (cddr form) new-env)))
    (setf (body ast)
	  (make-instance 'progn-ast :form-asts forms))
    ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CATCH.

(defclass catch-ast (ast)
  ((%tag :initarg :tag :reader tag)
   (%body :initarg :body :reader body)))

(defmethod convert-compound
    ((symbol (eql 'catch)) form environment)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 nil)
  (let ((forms (convert-sequence (cddr form) environment)))
    (make-instance 'catch-ast
      :tag (convert (cadr form) environment)
      :body (make-instance 'progn-ast :form-asts forms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting EVAL-WHEN.

(defclass eval-when-ast (ast)
  ((%situations :initarg :situations :reader situations)
   (%body :initarg :body :reader body)))

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
    (make-instance 'eval-when-ast
		   :situations (cadr form)
		   :body (make-instance 'progn-ast :form-asts forms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FLET.

(define-condition flet-functions-must-be-proper-list
    (compilation-program-error)
  ())

(define-condition functions-body-must-be-proper-list
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'flet)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 nil)
  (unless (sicl-code-utilities:proper-list-p (cadr form))
    (error 'flet-functions-must-be-proper-list
	   :expr form))
  (multiple-value-bind (local-vars local-codes)
      (loop for (name lambda-list . body) in (cadr form)
	    collect (sicl-env:make-local-function-entry name)
	      into vars
	    collect (convert-code lambda-list body env)
	      into codes
	    finally (return (values vars codes)))
    (multiple-value-bind (declarations forms)
	(sicl-code-utilities:separate-ordinary-body (cddr form))
      (let* ((new-env
	       (sicl-env:augment-environment-with-declarations
		(sicl-env:augment-environment env local-vars)
		declarations))
	     (body-asts (convert-sequence forms new-env))
	     (init-asts (loop for var in local-vars
			      for location = (sicl-env:location var)
			      for fun in local-codes
			      collect (make-instance 'setq-ast
						     :location location
						     :value-ast fun))))
	(make-instance 'progn-ast
		       :form-asts (append init-asts body-asts))))))

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
  (let ((entry (sicl-env:find-function name environment)))
    (make-instance 'typed-location-ast
		   :location (sicl-env:location entry)
		   :type (sicl-env:find-type entry environment))))

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

(defmethod convert-compound
    ((symbol (eql 'function)) form environment)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 1)
  (unless (or (proper-function-name-p (cadr form))
	      (and (consp (cadr form))
		   (eq (car (cadr form)) 'lambda)))
    (error 'function-argument-must-be-function-name-or-lambda-expression
	   :expr (cadr form)))
  (if (proper-function-name-p (cadr form))
      (convert-named-function (cadr form) environment)
      (convert-lambda-function (cadr form) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting GO.

(defclass go-ast (ast)
  ((%tag-ast :initarg :tag-ast :reader tag-ast)))

(defmethod convert-compound
    ((symbol (eql 'go)) form environment)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 1)
  (let ((entry (sicl-env:find-go-tag (cadr form) environment)))
    (if (null entry)
	(warn "undefined go tag: ~s" form)
	(make-instance 'go-ast
		       :tag-ast (sicl-env:definition entry)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting IF.

(defclass if-ast (ast)
  ((%test :initarg :test :reader test)
   (%then :initarg :then :reader then)
   (%else :initarg :else :reader else)))

(defmethod convert-compound
    ((symbol (eql 'if)) form environment)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 3)
  (make-instance 'if-ast
		 :test (convert (cadr form) environment)
		 :then (convert (caddr form) environment)
		 :else (if (null (cdddr form))
			   (convert-constant nil)
			   (convert (cadddr form) environment))))

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
	       ,(mapcar #'init-form bindings))
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

(defclass load-time-value-ast (ast)
  ((%form-ast :initarg :form-ast :reader form-ast)
   (%read-only-p :initarg :read-only-p :reader read-only-p)))

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
  (make-instance 'load-time-value-ast
		 :form-ast (convert (cadr form) environment)
		 :read-only-p (caddr form)))

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
      (make-instance 'progn-ast
		     :form-asts (convert-sequence forms new-env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MACROLET.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MULTIPLE-VALUE-CALL.

(defclass multiple-value-call-ast (ast)
  ((%function-ast :initarg :function-ast :reader function-ast)
   (%argument-asts :initarg :argument-asts :reader argument-asts)))

(defmethod convert-compound
    ((symbol (eql 'multiple-value-call)) form environment)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 nil)
  (make-instance 'multiple-value-call-ast
		 :function-ast (convert (cadr form) environment)
		 :argument-asts (convert-sequence (cddr form) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MULTIPLE-VALUE-PROG1.

(defclass multiple-value-prog1-ast (ast)
  ((%first-ast :initarg :first-ast :reader first-ast)
   (%body-asts :initarg :body-asts :reader body-asts)))

(defmethod convert-compound
    ((symbol (eql 'multiple-value-prog1)) form environment)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 nil)
  (make-instance 'multiple-value-prog1-ast
		 :first-ast (convert (cadr form) environment)
		 :body-asts (convert-sequence (cddr form) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting PROGN.

(defclass progn-ast (ast)
  ((%form-asts :initarg :form-asts :reader form-asts)))

(defmethod convert-compound
    ((head (eql 'progn)) form environment)
  (sicl-code-utilities:check-form-proper-list form)
  (make-instance 'progn-ast
		 :form-asts (convert-sequence (cdr form) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting PROGV.

(defclass progv-ast (ast)
  ((%symbols-ast :initarg :symbols-ast :reader symbols-ast)
   (%vals-ast :initarg :vals-ast :reader vals-ast)
   (%body-ast :initarg :body-ast :reader body-ast)))

(defmethod convert-compound
    ((symbol (eql 'progv)) form environment)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 nil)
  (let ((body-ast (make-instance 'progn-ast
		    :form-asts (convert-sequence (cdddr form) environment))))
    (make-instance 'progv-ast
		   :symbols-ast (convert (cadr form) environment)
		   :vals-ast (convert (caddr form) environment)
		   :body-ast body-ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting QUOTE.
;;;
;;; It shouldn't be necessary to convert quote here, because,
;;; CONSTANTP returns TRUE for a quoted form, so this case should have
;;; been caught by CONVERT itself.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting RETURN-FROM.

(defclass return-from-ast (ast)
  ((%block-ast :initarg :block-ast :reader block-ast)
   (%form-ast :initarg :form-ast :reader form-ast)))

(define-condition return-from-tag-must-be-symbol
    (compilation-program-error)
  ())

(define-condition return-from-tag-unknown
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'return-from)) form environment)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 2)
  (unless (symbolp (cadr form))
    (error 'return-from-tag-must-be-symbol
	   :expr (cadr form)))
  (let ((entry (sicl-env:find-block (cadr form) environment)))
    ;; FIXME: this currently can't happen, but maybe it should.
    (when (null entry)
      (error 'return-from-tag-unknown
	     :expr (cadr form)))
    (make-instance 'return-from-ast
		   :block-ast (sicl-env:definition entry)
		   :form-ast (convert (caddr form) environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SETQ.

(defclass setq-ast (ast)
  ((%location :initarg :location :reader location)
   (%value-ast :initarg :value-ast :reader value-ast)))

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

(defun convert-elementary-setq (var form environment)
  (unless (symbolp var)
    (error 'setq-var-must-be-symbol
	   :expr var))
  (let ((entry (sicl-env:find-variable var environment)))
    (cond ((null entry)
	   (error 'setq-unknown-variable
		  :form var))
	  ((typep entry 'sicl-env:constant-variable-entry)
	   (error 'setq-constant-variable
		  :form var))
	  ((or (typep entry 'sicl-env:lexical-variable-entry)
	       (typep entry 'sicl-env:special-variable-entry))
	   (make-instance 'setq-ast
			  :location (location entry)
			  :value-ast (convert form environment)))
	  ((typep entry 'sicl-env:symbol-macro-entry)
	   (convert `(setf ,(macroexpand var environment) form)
		    environment))
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
    (make-instance 'progn-ast
		   :form-asts form-asts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SYMBOL-MACROLET.

(defmethod convert-compound
    ((head (eql 'symbol-macrolet)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 nil)
  ;; FIXME: syntax check bindings
  (let* ((entries (loop for (name expansion) in (cadr form)
			collect (sicl-env:make-symbol-macro-entry
				 name 
				 (lambda (form env)
				   (declare (ignore form env))
				   expansion))))
	 (new-env (sicl-env:augment-environment env entries)))
    (convert `(progn ,@(cddr form)) new-env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting TAGBODY.

(defclass tag-ast (ast)
  ((%name :initarg :name :reader name)))

(defclass tagbody-ast (ast)
  ((%items :initarg :items :reader items)))

(defmethod convert-compound
    ((symbol (eql 'tagbody)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (let* ((tag-asts
	   (loop for item in (cdr form)
		 when (symbolp item)
		   collect (make-instance 'tag-ast
					  :name item)))
	 (entries
	   (loop for ast in tag-asts
		 collect (sicl-env:make-go-tag-entry (name ast) ast))))
    (let* ((new-env (sicl-env:augment-environment env entries))
	   (items (loop for item in (cdr form)
			collect (if (symbolp item)
				    (pop tag-asts)
				    (convert item new-env)))))
      (make-instance 'tagbody-ast
		     :items items))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting THE.

(defclass the-ast (ast)
  ((%value-type :initarg :value-type :reader value-type)
   (%form-ast :initarg :form-ast :reader form-ast)))

(defmethod convert-compound
    ((symbol (eql 'the)) form environment)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (make-instance 'the-ast
		 :value-type (cadr form)
		 :form-ast (convert (caddr form) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting THROW.

(defclass throw-ast (ast)
  ((%tag-ast :initarg :tag-ast :reader tag-ast)
   (%form-ast :initarg :form-ast :reader form-ast)))

(defmethod convert-compound
    ((symbol (eql 'throw)) form environment)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (make-instance 'throw-ast
		 :tag-ast (convert (cadr form) environment)
		 :form-ast (convert (caddr form) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting UNWIND-PROTECT.

(defclass unwind-protect-ast (ast)
  ((%protected-ast :initarg :protected-ast :reader protected-ast)
   (%cleanup-form-asts :initarg :cleanup-form-asts :reader cleanup-form-asts)))

(defmethod convert-compound
    ((symbol (eql 'unwind-protect)) form environment)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 nil)
  (make-instance 'unwind-protect-ast
		 :protected-ast (convert (cadr form) environment)
		 :cleanup-form-asts (convert-sequence (cddr form) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing an AST.

(defparameter *table* nil)

(defun id (ast)
  (symbol-name (gethash ast *table*)))

(defgeneric draw-ast (ast stream))

(defun draw (ast filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (format stream "digraph G {~%   ordering = out;~%")
    (let ((*table* (make-hash-table :test #'eq)))
      (draw-ast ast stream))
    (format stream "}~%")))

(defmethod draw-ast :around (ast stream)
  (when (null (gethash ast *table*))
    (setf (gethash ast *table*) (gensym))
    (format stream "  ~a [shape = box];~%"
	    (id ast))
    (call-next-method)))

(defmethod draw-ast ((ast constant-ast) stream)
  (format stream "   ~a [label = \"~a\"];~%"
	  (id ast)
	  (value ast)))

(defun draw-location (location stream)
  (when (null (gethash location *table*))
    (setf (gethash location *table*) (gensym))
    (format stream "  ~a [shape = circle];~%" (id location))
    (format stream "  ~a [label = \"\"];~%" (id location))))

(defmethod draw-ast ((ast typed-location-ast) stream)
  (format stream "   ~a [label = \"location\"];~%"
	  (id ast))
  (draw-location (location ast) stream)
  (format stream "   ~a -> ~a~%"
	  (id ast)
	  (id (location ast))))
  
(defmethod draw-ast ((ast if-ast) stream)
  (format stream "   ~a [label = \"if\"];~%"
	  (id ast))
  (draw-ast (test ast) stream)
  (draw-ast (then ast) stream)
  (draw-ast (else ast) stream)
  (format stream "   ~a -> ~a~%"
	  (id ast)
	  (id (test ast)))
  (format stream "   ~a -> ~a~%"
	  (id ast)
	  (id (then ast)))
  (format stream "   ~a -> ~a~%"
	  (id ast)
	  (id (else ast))))

(defmethod draw-ast ((ast progn-ast) stream)
  (format stream "   ~a [label = \"progn\"];~%"
	  (id ast))
  (loop for child in (form-asts ast)
	do (draw-ast child stream)
	   (format stream "   ~a -> ~a~%"
		   (id ast) (id child))))

(defmethod draw-ast ((ast function-call-ast) stream)
  (format stream "   ~a [label = \"funcall\"];~%"
	  (id ast))
  (let ((location (function-location ast)))
    (draw-location location stream)
    (format stream "   ~a -> ~a~%" (id ast) (id location)))
  (loop for child in (arguments ast)
	do (draw-ast child stream)
	   (format stream "   ~a -> ~a~%"
		   (id ast) (id child))))

(defmethod draw-ast ((ast block-ast) stream)
  (format stream "   ~a [label = \"block\"];~%"
	  (id ast))
  (draw-ast (body ast) stream)
  (format stream "   ~a -> ~a~%"
	  (id ast)
	  (id (body ast))))
  
(defmethod draw-ast ((ast return-from-ast) stream)
  (format stream "   ~a [label = \"return-from\"];~%"
	  (id ast))
  (format stream "   ~a -> ~a~%"
	  (id ast)
	  (id (block-ast ast)))
  (draw-ast (form-ast ast) stream)
  (format stream "   ~a -> ~a~%"
	  (id ast)
	  (id (form-ast ast))))
  
(defmethod draw-ast ((ast setq-ast) stream)
  (format stream "   ~a [label = \"setq\"];~%"
	  (id ast))
  (draw-location (location ast) stream)
  (format stream "   ~a -> ~a~%"
	  (id ast)
	  (id (location ast)))
  (draw-ast (value-ast ast) stream)
  (format stream "   ~a -> ~a~%"
	  (id ast)
	  (id (value-ast ast))))
  
(defmethod draw-ast ((ast function-ast) stream)
  (format stream "   ~a [label = \"function\"];~%"
	  (id ast))
  (loop for location in (required (lambda-list ast))
	do (draw-location location stream))
  (loop for location in (required (lambda-list ast))
	do (format stream "   ~a -> ~a~%"
		   (id ast)
		   (id location)))
  (draw-ast (body-ast ast) stream)
  (format stream "   ~a -> ~a~%"
	  (id ast)
	  (id (body-ast ast))))
  
(defmethod draw-ast ((ast tagbody-ast) stream)
  (format stream "   ~a [label = \"tagbody\"];~%"
	  (id ast))
  (loop for item in (items ast)
	do (draw-ast item stream)
	   (format stream "   ~a -> ~a~%"
		   (id ast) (id item))))

(defmethod draw-ast ((ast go-ast) stream)
  (format stream "   ~a [label = \"go\"];~%"
	  (id ast))
  (draw-ast (tag-ast ast) stream)
  (format stream "   ~a -> ~a~%"
	  (id ast) (id (tag-ast ast))))
