(defpackage #:sicl-compiler-phase-1
  (:use #:common-lisp)
  (:shadow #:proclaim
	   #:macroexpand
	   #:macroexpand-1
	   #:type
	   #:macro-function
	   #:*macroexpand-hook*
	   #:constantp)
  (:export
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
;;; Environment entries.

(defclass environment-entry ()
  ())

(defclass dummy-entry (environment-entry)
  ())

(defclass global-mixin ()
  ())

(defclass local-mixin ()
  ())

(defclass binding-entry (environment-entry)
  ((%name :initarg :name :reader name)))

(defclass variable-entry (binding-entry)
  ())

(defclass constant-variable-entry (variable-entry)
  ((%value :initarg :value :reader value)))

(defclass special-variable-entry (variable-entry)
  ())

(defclass global-special-variable-entry (special-variable-entry global-mixin)
  ())

(defclass local-special-variable-entry (special-variable-entry local-mixin)
  ())

(defclass lexical-variable-entry (variable-entry)
  ((%storage :initform (list nil) :reader storage)))

(defclass symbol-macro-entry (variable-entry)
  ((%expander :initarg :expander :accessor expander)))

(defclass global-symbol-macro-entry (symbol-macro-entry global-mixin)
  ())

(defclass local-symbol-macro-entry (symbol-macro-entry local-mixin)
  ())

(defclass function-or-macro-entry (binding-entry)
  ())

(defclass function-entry (function-or-macro-entry)
  ((%storage :initform (list nil) :reader storage)))

(defclass global-function-entry (function-entry global-mixin)
  ())
  
(defclass local-function-entry (function-entry local-mixin)
  ())
  
(defclass macro-entry (function-or-macro-entry)
  ((%expander :initarg :expander :accessor expander)))

(defclass global-macro-entry (macro-entry global-mixin)
  ())
  
(defclass local-macro-entry (macro-entry local-mixin)
  ())
  
(defclass block-entry (binding-entry)
  ((%storage :initform (list nil) :reader storage)))

(defclass go-tag-entry (binding-entry)
  ((%storage :initform (list nil) :reader storage)))

(defclass declaration-entry (environment-entry) ())

(defclass binding-declaration-entry (declaration-entry)
  ((%binding :initarg :binding :reader binding)))

(defclass type-declaration-entry (binding-declaration-entry)
  ((%type :initarg :type :reader type)))

(defclass inline-or-notinline-declaration-entry (binding-declaration-entry)
  ())

(defclass inline-declaration-entry (inline-or-notinline-declaration-entry)
  ())

(defclass notinline-declaration-entry (inline-or-notinline-declaration-entry)
  ())

(defclass dynamic-extent-declaration-entry (binding-declaration-entry)
  ())

(defclass ignore-declaration-entry (binding-declaration-entry)
  ())

(defclass ignorable-declaration-entry (binding-declaration-entry)
  ())

(defclass autonomous-declaration-entry (declaration-entry)
  ())

(defclass optimize-declaration-entry (autonomous-declaration-entry)
  ((%quality :initarg :quality)
   (%value :initarg :value)))

(defclass declaration-declaration-entry (autonomous-declaration-entry)
  ((%name :initarg :name :reader name)))

(defparameter *global-environment*
  (list (make-instance 'dummy-entry)))

(defun add-to-global-environment (entry)
  (push entry (cdr *global-environment*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Create an environment entry from a canonicalized declaration
;;; specifier.

(defun find-variable-binding (name environment)
  (let ((entry (find-if (lambda (entry)
			  (and (or (typep entry 'lexical-variable-entry)
				   (typep entry 'special-variable-entry)
				   (typep entry 'constant-variable-entry))
			       (eq (name entry) name)))
			environment)))
    (when (null entry)
      ;; FIXME: do this better
      (error "no such variable"))
    (binding entry)))

(defun find-function-binding (name environment)
  (let ((entry (find-if (lambda (entry)
			  (and (typep entry 'function-entry)
			       (eq (name entry) name)))
			environment)))
    (when (null entry)
      ;; FIXME: do this better
      (error "no such function"))
    (binding entry)))

(defun make-environment-entry (canonicalized-delcaration-specifier environment)
  (destructuring-bind (head . rest) canonicalized-delcaration-specifier
    (case head
      (declaration
       (make-instance 'declaration-declaration-entry
		      :name (car rest)))
      (dynamic-extent
       (let ((binding (if (consp (car rest))
			  (find-function-binding (cadr (car rest)) environment)
			  (find-variable-binding (car rest) environment))))
	 (make-instance 'dynamic-extent-declaration-entry
			:binding binding)))
      (ftype
       (let ((binding (find-function-binding (cadr rest) environment)))
	 (make-instance 'type-declaration-entry
			:binding binding
			:type (car rest))))
      (ignorable
       (let ((binding (if (consp (car rest))
			  (find-function-binding (cadr (car rest)) environment)
			  (find-variable-binding (car rest) environment))))
	 (make-instance 'ignorable-declaration-entry
			:binding binding)))
      (ignore
       (let ((binding (if (consp (car rest))
			  (find-function-binding (cadr (car rest)) environment)
			  (find-variable-binding (car rest) environment))))
	 (make-instance 'ignore-declaration-entry
			:binding binding)))
      (inline
       (let ((binding (find-function-binding (car rest) environment)))
	 (make-instance 'inline-declaration-entry
			:binding binding)))
      (notinline
       (let ((binding (find-function-binding (car rest) environment)))
	 (make-instance 'notinline-declaration-entry
			:binding binding)))
      (optimize
       (make-instance 'optimize-declaration-entry
		      :quality (car (car rest))
		      :value (cadr (car rest))))
      (special
       (make-instance 'special-variable-entry
		      :name (car rest)))
      (type
       (let ((binding (find-variable-binding (cadr rest) environment)))
	 (make-instance 'type-declaration-entry
			:binding binding
			:type (car rest)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function PROCLAIM.

(defun proclaim-declaration (name)
  (unless (find-if (lambda (entry)
		     (and (typep entry 'declaration-declaration-entry)
			  (eq (name entry) name)))
		   *global-environment*)
    (add-to-global-environment
     (make-instance 'declaration-declaration-entry
		    :name name))))

(defun proclaim-ftype (name type)
  (let ((binding (find-if (lambda (binding)
			    (and (typep binding 'global-function-entry)
				 (eq (name binding) name)))
			  *global-environment*)))
    (when (null binding)
      (error "no function by that name"))
    (let ((existing-declaration
	    (find-if (lambda (decl)
		       (and (typep decl 'type-declaration-entry)
			    (eq (binding decl) binding)))
		     *global-environment*)))
      (cond ((null existing-declaration)
	     (add-to-global-environment
	      (make-instance 'type-declaration-entry
			     :binding binding
			     :type type)))
	    ((equal (type existing-declaration) type)
	     nil)
	    (t
	     ;; make that an error for now
	     (error "function already has a type proclamation"))))))
	  

(defun proclaim (declaration-specifier)
  (case (car declaration-specifier)
    (declaration
     (mapc #'proclaim-declaration
	   (cdr declaration-specifier)))
    (ftype
     (mapc (lambda (name) (proclaim-ftype name (cadr declaration-specifier)))
	   (cddr declaration-specifier)))
    ;; FIXME: handle more proclamations
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro expansion.

(defun macro-function (symbol &optional environment)
  (when (null environment)
    (setf environment *global-environment*))
  (let ((binding (find-if (lambda (entry)
			    (and (typep entry 'macro-entry)
				 (eq (name entry) symbol)))
			  environment)))
    (if (null binding)
	nil
	(expander binding))))

(defparameter *macroexpand-hook*
  (lambda (macro-function macro-form environment)
    (funcall macro-function macro-form environment)))

(defun macroexpand-1 (form &optional environment)
  (when (null environment)
    (setf environment *global-environment*))
  (let ((expander nil))
    (cond ((and (consp form) (symbolp (car form)))
	   (setf expander (macro-function (car form) environment)))
	  ((symbolp form)
	   (let ((binding (find-if (lambda (entry)
				     (and (typep entry 'symbol-macro-entry)
					  (eq (name entry) form)))
				   environment)))
	     (if (null binding)
		 nil
		 (setf expander (expander binding)))))
	  (t nil))
    (if expander
	(values (funcall (coerce *macroexpand-hook* 'function)
			 expander
			 form
			 environment)
		t)
	(values form nil))))

(defun macroexpand (form &optional environment)
  (multiple-value-bind (expansion expanded-p)
      (macroexpand-1 form environment)
    (if expanded-p
	(loop while (multiple-value-bind (new-expansion expanded-p)
			(macroexpand-1 expansion environment)
		      (setf expansion new-expansion)
		      expanded-p)
	      finally (return (values expansion t)))
	(values form nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting code to an abstract syntax tree.

(defclass ast () ())

(defclass constant-ast (ast)
  ((%value :initarg :value :reader value)))

(defclass variable-ast (ast)
  ((%binding :initarg :binding :reader binding)
   (%type :initarg :type :reader type)))

(defun constantp (form &optional environment)
  ;; FIXME: consult the environment for
  ;; constants defined with defconstant.
  (when (null environment)
    (setf environment *global-environment*))
  (or (keywordp form)
      (member form '(nil t))
      (and (atom form) (not (symbolp form)))
      (and (consp form) (eq (car form) 'quote))))

(defun convert-constant (form)
  (make-instance 'constant-ast :value form))

(defun find-type (binding environment)
  `(and ,@(loop for entry in environment
		when (and (typep entry 'type-declaration-entry)
			  (eq (binding entry) binding))
		  collect (type entry))))

(defun convert-variable (form environment)
  (let ((binding (find-if (lambda (entry)
			    (and (typep entry 'variable)
				 (eq (name entry) form)))
			  environment)))
    (if (null binding)
	(warn "undefined variable: ~s" form)
	(make-instance 'variable-ast
		       :binding binding
		       :type (find-type binding environment)))))

(defgeneric convert-compound (head form environment))

(defun convert (form environment)
  (setf form (macroexpand form environment))
  (cond ((constantp form environment)
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
  ((%binding :initarg :binding :reader binding)
   (%type :initarg :type :reader type)
   (%arguments :initarg :arguments :reader arguments)))

(defmethod convert-compound ((head symbol) form environment)
  (let ((binding (find-if (lambda (entry)
			    (and (typep entry 'function-entry)
				 (eq (name entry) head)))
			  environment)))
    (if (null binding)
	(warn "no such function")
	(make-instance 'function-call-ast
		       :binding binding
		       :type (find-type binding environment)
		       :arguments (loop for arg in (cdr form)
					collect (convert arg environment))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting BLOCK.

(defclass block-ast (ast)
  ((%binding :initarg :binding :reader binding)
   (%forms :initarg :forms :reader forms)))

(define-condition block-special-form-must-be-proper-list 
    (compilation-program-error)
  ())

(define-condition block-must-have-at-least-one-argument
    (compilation-program-error)
  ())

(define-condition block-name-must-be-a-symbol
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'block)) form environment)
  (unless (sicl-code-utilities:proper-list-p form)
    (error 'block-special-form-must-be-proper-list
	   :expr form))
  (unless (>= (length form) 2)
    (error 'block-must-have-at-least-one-argument
	   :expr form))
  (unless (symbolp (cadr form))
    (error 'block-name-must-be-a-symbol
	   :expr (cadr form)))
  (let ((binding (make-instance 'block-entry
				:name (cadr form))))
    (make-instance 'block-ast
      :name binding
      :forms (convert-sequence (cddr form) (cons binding environment)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CATCH.

(defclass catch-ast (ast)
  ((%tag :initarg :tag :reader tag)
   (%forms :initarg :forms :reader forms)))

(define-condition catch-special-form-must-be-proper-list 
    (compilation-program-error)
  ())

(define-condition catch-must-have-at-least-one-argument
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'catch)) form environment)
  (unless (sicl-code-utilities:proper-list-p form)
    (error 'catch-special-form-must-be-proper-list
	   :expr form))
  (unless (>= (length form) 2)
    (error 'catch-must-have-at-least-one-argument
	   :expr form))
  (make-instance 'catch-ast
		 :tag (convert (cadr form) environment)
		 :forms (convert-sequence (cddr form) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting EVAL-WHEN.

(defclass eval-when-ast (ast)
  ((%situations :initarg :situations :reader situations)
   (%forms :initarg :forms :reader forms)))

(define-condition eval-when-special-form-must-be-proper-list 
    (compilation-program-error)
  ())

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
  (unless (sicl-code-utilities:proper-list-p form)
    (error 'eval-when-special-form-must-be-proper-list
	   :expr form))
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
  (make-instance 'eval-when-ast
		 :situations (cadr form)
		 :forms (convert-sequence (cddr form) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FLET.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FUNCTION.

(defclass function-ast (ast)
  ((%binding :initarg :binding :reader binding)
   (%type :initarg :type :reader type)))

(define-condition function-special-form-must-be-proper-list 
    (compilation-program-error)
  ())

(define-condition function-must-have-exactly-one-argument
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'function)) form environment)
  (unless (sicl-code-utilities:proper-list-p form)
    (error 'function-special-form-must-be-proper-list
	   :expr form))
  (unless (= (length form) 2)
    (error 'function-must-have-exactly-one-argument
	   :expr form))
  (let ((binding (find-if (lambda (entry)
			    (and (typep entry 'function-entry)
				 (eq (name entry) form)))
			  environment)))
    (if (null binding)
	(warn "undefined function: ~s" form)
	(make-instance 'function-ast
		       :binding binding
		       :type (find-type binding environment)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting GO.

(defclass go-ast (ast)
  ((%binding :initarg :binding :reader binding)))

(define-condition go-special-form-must-be-proper-list 
    (compilation-program-error)
  ())

(define-condition go-must-have-exactly-one-argument
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'go)) form environment)
  (unless (sicl-code-utilities:proper-list-p form)
    (error 'go-special-form-must-be-proper-list
	   :expr form))
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

(define-condition if-special-form-must-be-proper-list 
    (compilation-program-error)
  ())

(define-condition if-must-have-three-or-four-arguments
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'if)) form environment)
  (unless (sicl-code-utilities:proper-list-p form)
    (error 'if-special-form-must-be-proper-list
	   :expr form))
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

(define-condition let-special-form-must-be-proper-list 
    (compilation-program-error)
  ())

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
  (unless (sicl-code-utilities:proper-list-p form)
    (error 'let-special-form-must-be-proper-list
	   :expr form))
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

(define-condition load-time-value-special-form-must-be-proper-list 
    (compilation-program-error)
  ())

(define-condition load-time-value-must-have-one-or-two-arguments
    (compilation-program-error)
  ())

(define-condition read-only-p-must-be-boolean
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'load-time-value)) form environment)
  (unless (sicl-code-utilities:proper-list-p form)
    (error 'load-time-value-special-form-must-be-proper-list
	   :expr form))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MACROLET.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MULTIPLE-VALUE-CALL.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MULTIPLE-VALUE-PROG1.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting PROGN.

(defclass progn-ast (ast)
  ((%forms :initarg :forms :reader forms)))

(define-condition progn-special-form-must-be-proper-list 
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((head (eql 'progn)) form environment)
  (unless (sicl-code-utilities:proper-list-p form)
    (error 'progn-special-form-must-be-proper-list
	   :expr form))
  (make-instance 'progn-ast
		 :forms (loop for subform in (cdr form)
			      collect (convert subform environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting PROGV.

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

(define-condition return-from-special-form-must-be-proper-list 
    (compilation-program-error)
  ())

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
  (unless (sicl-code-utilities:proper-list-p form)
    (error 'return-from-special-form-must-be-proper-list
	   :expr form))
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

(define-condition setq-special-form-must-be-proper-list 
    (compilation-program-error)
  ())

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
  (unless (sicl-code-utilities:proper-list-p form)
    (error 'setq-special-form-must-be-proper-list
	   :expr form))
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

(define-condition macrolet-special-form-must-be-proper-list 
    (compilation-program-error)
  ())

(define-condition macrolet-must-have-at-least-one-argument
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((head (eql 'symbol-macrolet)) form environment)
  (unless (sicl-code-utilities:proper-list-p form)
    (error 'macrolet-special-form-must-be-proper-list
	   :form form))
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

(define-condition tagbody-special-form-must-be-proper-list 
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'tagbody)) form environment)
  (unless (sicl-code-utilities:proper-list-p form)
    (error 'tagbody-special-form-must-be-proper-list
	   :form))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting THROW.

(defclass throw-ast (ast)
  ((%tag :initarg :tag :reader tag)
   (%form :initarg :form :reader form)))

(define-condition throw-special-form-must-be-proper-list 
    (compilation-program-error)
  ())

(define-condition throw-must-have-exactly-two-arguments
    (compilation-program-error)
  ())

(defmethod convert-compound
    ((symbol (eql 'throw)) form environment)
  (unless (sicl-code-utilities:proper-list-p form)
    (error 'throw-special-form-must-be-proper-list
	   :expr form))
  (unless (= (length form) 3)
    (error 'throw-must-have-exactly-two-arguments
	   :expr form))
  (make-instance 'throw-ast
		 :tag (convert (cadr form) environment)
		 :form (convert-sequence (caddr form) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting UNWIND-PROTECT.

