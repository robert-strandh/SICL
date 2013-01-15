(defpackage #:sicl-compiler-phase-1
  (:use #:common-lisp)
  (:shadow #:type
   )
  (:export
   #:ast
   #:constant-ast #:value
   #:typed-location-ast
   #:function-call-ast #:binding #:arguments
   #:block-ast #:binding #:body
   #:catch-ast
   #:eval-when-ast
   #:function-ast
   #:go-ast
   #:if-ast #:test #:then #:else
   #:load-time-value-ast
   #:progn-ast #:forms
   #:return-from-ast #:form
   #:setq-ast
   #:tagbody-ast #:items
   #:the-ast
   #:throw-ast
   #:unwind-protect-ast
   ))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Specific condition classes.

(define-condition special-form-must-be-proper-list
    (compilation-program-error)
  ())

(defun check-special-form-proper-list (form)
  (unless (sicl-code-utilities:proper-list-p form)
    (error 'special-form-must-be-proper-list
	   :expr form)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting BLOCK.

(defclass block-ast (ast)
  ((%body :initarg :body :reader body)))

(define-condition block-must-have-at-least-one-argument
    (compilation-program-error)
  ())

(define-condition block-name-must-be-a-symbol
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'block)) form env)
  (check-special-form-proper-list form)
  (unless (>= (length form) 2)
    (error 'block-must-have-at-least-one-argument
	   :expr form))
  (unless (symbolp (cadr form))
    (error 'block-name-must-be-a-symbol
	   :expr (cadr form)))
  (let* ((entry (sicl-env:make-block-entry (cadr form)))
	 (new-env (sicl-env:augment-environment env entry))
	 (forms (convert-sequence (cddr form) new-env)))
    (make-instance 'block-ast
      :body (make-instance 'progn-ast :forms forms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CATCH.

(defclass catch-ast (ast)
  ((%tag :initarg :tag :reader tag)
   (%body :initarg :body :reader body)))

(define-condition catch-must-have-at-least-one-argument
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'catch)) form environment)
  (check-special-form-proper-list form)
  (unless (>= (length form) 2)
    (error 'catch-must-have-at-least-one-argument
	   :expr form))
  (let ((forms (convert-sequence (cddr form) environment)))
    (make-instance 'catch-ast
      :tag (convert (cadr form) environment)
      :body (make-instance 'progn-ast :forms forms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting EVAL-WHEN.

(defclass eval-when-ast (ast)
  ((%situations :initarg :situations :reader situations)
   (%body :initarg :body :reader body)))

(define-condition eval-when-must-have-at-least-one-argument
    (compilation-program-error)
  ())

(define-condition situations-must-be-proper-list
    (compilation-program-error)
  ())

(define-condition invalid-eval-when-situation
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'eval-when)) form environment)
  (check-special-form-proper-list form)
  (unless (>= (length form) 2)
    (error 'eval-when-must-have-at-least-one-argument
	   :expr form))
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
		   :body (make-instance 'progn-ast :forms forms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FLET.

(defun augment-environment-with-declarations (declarations environment)
  (let* ((declaration-specifiers
	   (sicl-code-utilities:canonicalize-declaration-specifiers
	    (reduce #'append (mapcar #'cdr declarations))))
	 (entries (mapcar #'make-environment-entry declaration-specifiers)))
    (append entries environment)))

(define-condition flet-must-have-at-least-one-argument
    (compilation-program-error)
  ())

(define-condition flet-functions-must-be-proper-list
    (compilation-program-error)
  ())

(define-condition functions-body-must-be-proper-list
    (compilation-program-error)
  ())

(defun convert-function (lambda-list body environment)
  (let* ((parsed-lambda-list
	   (sicl-code-utilities:parse-ordinary-lambda-list lambda-list))
	 (vars (sicl-code-utilities:lambda-list-variables parsed-lambda-list)))
    (multiple-value-bind (declarations documentation forms)
	(sicl-code-utilities:separate-function-body body)
      (declare (ignore documentation))
      (let* ((new-environment
	       (augment-environment-with-declarations
		declarations
		(append (loop for var in vars
			      collect (make-instance 'lexical-variable-entry
						    :name var))
			environment)))
	     (body-asts (convert-sequence forms new-environment)))
	(make-instance 'close-ast
		       ;; FIXME: Need to convert the lambda list first.
		       :lambda-list parsed-lambda-list
		       :body (make-instance 'progn-ast
					    :forms body-asts))))))

(defmethod convert-compound
    ((symbol (eql 'flet)) form environment)
  (check-special-form-proper-list form)
  (unless (>= (length form) 2)
    (error 'flet-must-have-at-least-one-argument
	   :expr form))
  (unless (sicl-code-utilities:proper-list-p (cadr form))
    (error 'flet-functions-must-be-proper-list
	   :expr form))
  (multiple-value-bind (local-vars local-functions)
      (loop for (name lambda-list body) in (cadr form)
	    collect (make-instance 'lexical-variable-entry :name name)
	      into vars
	    collect (convert-function lambda-list body environment)
	      into funs
	    finally (return (values vars funs)))
    (multiple-value-bind (declarations forms)
	(sicl-code-utilities:separate-ordinary-body (cddr form))
      (let* ((new-environment
	       (augment-environment-with-declarations
		declarations
		(append local-vars environment)))
	     (body-asts (convert-sequence forms new-environment))
	     (init-asts (loop for var in local-vars
			      for fun in local-functions
			      collect (make-instance 'setq-ast
						     :binding var
						     :value fun))))
	(make-instance 'progn-ast
		       :forms (append init-asts body-asts))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FUNCTION.

(defclass close-ast (ast)
  ((%lambda-list :initarg :lambda-list :reader lambda-list)
   (%body :initarg :body :reader body)))

(define-condition function-must-have-exactly-one-argument
    (compilation-program-error)
  ())

(define-condition lambda-must-be-proper-list
    (compilation-program-error)
  ())


(defun convert-named-function (name environment)
  (let ((binding (find-if (lambda (entry)
			    (and (typep entry 'function-entry)
				 (eq (name entry) name)))
			  environment)))
    (if (null binding)
	(warn "undefined function: ~s" name)
	(make-instance 'typed-location-ast
		       :binding binding
		       :type (find-type binding environment)))))

(defun convert-lambda-function (lambda-form environment)
  (unless (sicl-code-utilities:proper-list-p lambda-form)
    (error 'lambda-must-be-proper-list
	   :expr lambda-form))
  (let* ((parsed-lambda-list
	   (sicl-code-utilities:parse-ordinary-lambda-list (cadr lambda-form)))
	 (vars (sicl-code-utilities:lambda-list-variables parsed-lambda-list))
	 (new-environment
	   (append (loop for var in vars
			 collect (make-instance 'lexical-variable-entry
						:name var))
		   environment)))
    (let ((body (convert-sequence (cddr lambda-form) new-environment)))
      (make-instance 'close-ast
		     ;; FIXME: Need to convert the lambda list first.
		     :lambda-list parsed-lambda-list
		     :body (make-instance 'progn-ast
					  :forms body)))))

(defmethod convert-compound
    ((symbol (eql 'function)) form environment)
  (check-special-form-proper-list form)
  (unless (= (length form) 2)
    (error 'function-must-have-exactly-one-argument
	   :expr form))
  (if (symbolp (cadr form))
      (convert-named-function (cadr form) environment)
      (convert-lambda-function (cadr form) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting GO.

(defclass go-ast (ast)
  ((%binding :initarg :binding :reader binding)))

(define-condition go-must-have-exactly-one-argument
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'go)) form environment)
  (check-special-form-proper-list form)
  (unless (= (length form) 2)
    (error 'go-must-have-exactly-one-argument
	   :expr form))
  (let ((binding (find-if (lambda (entry)
			    (and (typep entry 'go-tag-entry)
				 (eq (name entry) (cadr form))))
			  environment)))
    (if (null binding)
	(warn "undefined go tag: ~s" form)
	(make-instance 'go-ast
		       :binding binding))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting IF.

(defclass if-ast (ast)
  ((%test :initarg :test :reader test)
   (%then :initarg :then :reader then)
   (%else :initarg :else :reader else)))

(define-condition if-must-have-three-or-four-arguments
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'if)) form environment)
  (check-special-form-proper-list form)
  (unless (<= 3 (length form) 4)
    (error 'if-must-have-three-or-four-arguments
	   :expr form))
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

(define-condition let-must-have-at-least-one-argument
    (compilation-program-error)
  ())

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

(defun init-form (binding-form)
  (if (or (symbolp binding-form) (null (cdr binding-form)))
      nil
      (cadr binding-form)))

;;; FIXME: handle special variables.
;;; FIXME: handle declarations.
(defmethod convert-compound
    ((symbol (eql 'let)) form environment)
  (check-special-form-proper-list form)
  (unless (>= (length form) 2)
    (error 'let-must-have-at-least-one-argument
	   :expr form))
  (check-binding-forms (cadr form))
  (let ((inits (loop for if in (cadr form)
		     collect (convert (init-form if) environment)))
	(bindings (loop for if in (cadr form)
			collect (make-instance 'lexical-variable-entry
					       :name (if (symbolp if)
							 if
							 (car if)))))
	(new-environment environment))
    (loop for binding in bindings
	  do (push binding new-environment))
    (let ((forms (loop for binding in bindings
		       for init in inits
		       collect (make-instance 'setq-ast
					      :binding binding
					      :value init))))
      (make-instance 'progn-ast
		     :forms forms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LET*.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LOAD-TIME-VALUE.

(defclass load-time-value-ast (ast)
  ((%form :initarg :form :reader form)
   (%read-only-p :initarg :read-only-p :reader read-only-p)))

(define-condition load-time-value-must-have-one-or-two-arguments
    (compilation-program-error)
  ())

(define-condition read-only-p-must-be-boolean
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'load-time-value)) form environment)
  (check-special-form-proper-list form)
  (unless (<= 2 (length form) 3)
    (error 'load-time-value-must-have-one-or-two-arguments
	   :expr form))
  (unless (null (cddr form))
    ;; The HyperSpec specifically requires a "boolean"
    ;; and not a "generalized boolean".
    (unless (member (caddr form) '(nil t))
      (error 'read-only-p-must-be-boolean
	     :expr (caddr form))))
  (make-instance 'load-time-value-ast
		 :form (convert (cadr form) environment)
		 :read-only-p (caddr form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LOCALLY.

(defmethod convert-compound
    ((symbol (eql 'locally)) form environment)
  (check-special-form-proper-list form)
  (multiple-value-bind (declarations forms)
      (sicl-code-utilities:separate-ordinary-body (cdr form))
    (let* ((declaration-specifiers
	     (sicl-code-utilities:canonicalize-declaration-specifiers
	      (reduce #'append (mapcar #'cdr declarations))))
	   (entries (mapcar #'make-environment-entry declaration-specifiers))
	   (new-environment (append entries environment)))
      (make-instance 'progn-ast
		     :forms (convert-sequence forms new-environment)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MACROLET.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MULTIPLE-VALUE-CALL.

(defclass multiple-value-call-ast (ast)
  ((%function-form :initarg :function-form :reader function-form)
   (%forms :initarg :forms :reader forms)))

(define-condition multiple-value-call-must-have-at-least-one-argument
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'multiple-value-call)) form environment)
  (check-special-form-proper-list form)
  (unless (>= (length form) 2)
    (error 'multiple-value-call-must-have-at-least-one-argument
	   :expr form))
  (make-instance 'multiple-value-call-ast
		 :function-form (convert (cadr form) environment)
		 :forms (convert-sequence (cddr form) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MULTIPLE-VALUE-PROG1.

(defclass multiple-value-prog1-ast (ast)
  ((%first-form :initarg :first-form :reader first-form)
   (%forms :initarg :forms :reader forms)))

(define-condition multiple-value-prog1-must-have-at-least-one-argument
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'multiple-value-prog1)) form environment)
  (check-special-form-proper-list form)
  (unless (>= (length form) 2)
    (error 'multiple-value-prog1-must-have-at-least-one-argument
	   :expr form))
  (make-instance 'multiple-value-prog1-ast
		 :first-form (convert (cadr form) environment)
		 :forms (convert-sequence (cddr form) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting PROGN.

(defclass progn-ast (ast)
  ((%forms :initarg :forms :reader forms)))

(defmethod convert-compound
    ((head (eql 'progn)) form environment)
  (check-special-form-proper-list form)
  (make-instance 'progn-ast
		 :forms (convert-sequence (cdr form) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting PROGV.

(defclass progv-ast (ast)
  ((%symbols :initarg :symbols :reader symbols)
   (%vals :initarg :vals :reader vals)
   (%body :initarg :body :reader body)))

(define-condition progv-must-have-at-least-two-arguments
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'progv)) form environment)
  (check-special-form-proper-list form)
  (unless (>= (length form) 3)
    (error 'progv-must-have-at-least-two-arguments
	   :expr form))
  (let ((body (make-instance 'progn-ast
		:forms (convert-sequence (cdddr form) environment))))
    (make-instance 'progv-ast
		   :symbols (convert (cadr form) environment)
		   :vals (convert (caddr form) environment)
		   :body body)))

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
  ((%binding :initarg :binding :reader binding)
   (%form :initarg :form :reader form)))

(define-condition return-from-must-have-one-or-two-arguments
    (compilation-program-error)
  ())

(define-condition return-from-tag-must-be-symbol
    (compilation-program-error)
  ())

(define-condition return-from-tag-unknown
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'return-from)) form environment)
  (check-special-form-proper-list form)
  (unless (<= 2 (length form) 3)
    (error 'return-from-must-have-one-or-two-arguments
	   :expr form))
  (unless (symbolp (cadr form))
    (error 'return-from-tag-must-be-symbol
	   :expr (cadr form)))
  (let ((entry (find-if (lambda (entry)
			  (typep entry 'block-entry)
			  (eq (name entry) (cadr form)))
			environment)))
    (when (null entry)
      (error 'return-from-tag-unknown
	     :expr (cadr form)))
    (make-instance 'return-from-ast
		   :binding (binding entry)
		   :form (convert (cadr form) environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SETQ.

(defclass setq-ast (ast)
  ((%binding :initarg :binding :reader binding)
   (%value :initarg :value :reader value)))

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
  (let ((entry (find-if (lambda (entry)
			  (and (or (typep entry 'variable-entry))
			       (eq (name entry) var)))
			environment)))
    (cond ((null entry)
	   (error 'setq-unknown-variable
		  :form var))
	  ((typep entry 'constant-variable-entry)
	   (error 'setq-constant-variable
		  :form var))
	  ((or (typep entry 'lexical-variable-entry)
	       (typep entry 'special-variable-entry))
	   (make-instance 'setq-ast
			  :binding (binding entry)
			  :form form))
	  ((typep entry 'symbol-macro-entry)
	   (convert `(setf ,(macroexpand var environment) form)
		    environment))
	  (t
	   (error "this should not happen")))))
  
(defmethod convert-compound
    ((symbol (eql 'setq)) form environment)
  (check-special-form-proper-list form)
  (unless (oddp (length form))
    (error 'setq-must-have-even-number-of-arguments
	   :expr form))
  (let ((forms (loop for (var form) on (cdr form) by #'cddr
		     collect (convert-elementary-setq var form environment))))
    (make-instance 'progn-ast
		   :forms forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SYMBOL-MACROLET.

(define-condition macrolet-must-have-at-least-one-argument
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((head (eql 'symbol-macrolet)) form environment)
  (check-special-form-proper-list form)
  (unless (>= (length form) 2)
    (error 'macrolet-must-have-at-least-one-argument
	   :form form))
  ;; FIXME: syntax check bindings
  (let ((new-environment environment))
    (loop for (name expansion) in (cadr form)
	  do (push (make-instance 'local-symbol-macro-entry
		     :name name
		     :expander (lambda (form environment)
				 (declare (ignore form environment))
				 expansion))
		   new-environment))
    (convert `(progn ,@(cddr form)) new-environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting TAGBODY.

(defclass tagbody-ast (ast)
  ((%items :initarg :items :reader items)))

(defmethod convert-compound
    ((symbol (eql 'tagbody)) form environment)
  (check-special-form-proper-list form)
  (let ((tag-entries
	  (loop for item in (cdr form)
		when (symbolp item)
		  collect (make-instance 'go-tag-entry
					 :name item))))
    (let ((new-environment (append tag-entries environment)))
      (let ((items (loop for item in (cdr form)
			 collect (if (symbolp item)
				     (pop tag-entries)
				     (convert item new-environment)))))
	(make-instance 'tagbody-ast
		       :items items)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting THE.

(defclass the-ast (ast)
  ((%value-type :initarg :value-type :reader value-type)
   (%form :initarg :form :reader form)))

(define-condition the-must-have-exactly-two-arguments
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'the)) form environment)
  (check-special-form-proper-list form)
  (unless (= (length form) 3)
    (error 'the-must-have-exactly-two-arguments
	   :expr form))
  (make-instance 'the-ast
		 :value-type (cadr form)
		 :form (convert (caddr form) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting THROW.

(defclass throw-ast (ast)
  ((%tag :initarg :tag :reader tag)
   (%form :initarg :form :reader form)))

(define-condition throw-must-have-exactly-two-arguments
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'throw)) form environment)
  (check-special-form-proper-list form)
  (unless (= (length form) 3)
    (error 'throw-must-have-exactly-two-arguments
	   :expr form))
  (make-instance 'throw-ast
		 :tag (convert (cadr form) environment)
		 :form (convert-sequence (caddr form) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting UNWIND-PROTECT.

(defclass unwind-protect-ast (ast)
  ((%protected-form :initarg :protected-form :reader protected-form)
   (%cleanup-forms :initarg :cleanup-forms :reader cleanup-forms)))

(define-condition unwind-protect-must-have-at-least-one-argument
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'unwind-protect)) form environment)
  (check-special-form-proper-list form)
  (unless (>= (length form) 2)
    (error 'unwind-protect-must-have-at-least-one-argument
	   :expr form))
  (make-instance 'unwind-protect-ast
		 :protected-form (convert (cadr form) environment)
		 :cleanup-forms (convert-sequence (cddr form) environment)))
