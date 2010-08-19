(defpackage #:sicl-compiler-phase-1
    (:use #:common-lisp)
  (:shadow #:variable))

(in-package #:sicl-compiler-phase-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Printer programming

(defclass compiler-object () ())

(defmethod print-object ((obj compiler-object) stream)
  (pprint-logical-block (stream nil :prefix "[" :suffix "]")
    (format stream "~s ~2i" (class-name (class-of obj)))
    (loop for slot in (sb-mop:class-slots (class-of obj))
	  do (format stream "~_~s ~W "
		     (car (sb-mop:slot-definition-initargs slot))
		     (if (slot-boundp obj (sb-mop:slot-definition-name slot))
			 (slot-value obj (sb-mop:slot-definition-name slot))
			 "no-value")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Internal representation of source code

;;; The first step is to convert the raw Lisp form into an 
;;; abstract syntax tree, so that we can then use generic
;;; function dispatch for the following steps. 

(defclass form (compiler-object)
  ((%original-form :initarg :original-form :accessor original-form)))

(defclass symbol-form (form) ())  

(defclass constant-form (form) ())

(defclass compound-form (form)
  ((%subforms :initarg :subforms :reader subforms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Environment

(defclass namespace (compiler-object)
  ((%bindings :initarg :bindings :accessor bindings)))

(defgeneric lookup-in-namespace (name namespace))

(defgeneric add-binding (name value namespace))

;;; A namespace where the bindings are organized as
;;; an association list. 
(defclass alist-namespace (namespace)
  ()
  (:default-initargs :bindings nil))

(defmethod lookup-in-namespace (name (namespace alist-namespace))
  (cdr (assoc name (bindings namespace))))

(defmethod add-binding (name value (namespace alist-namespace))
  (push (cons name value) (bindings namespace)))

;;; A namespace where the bindings are organized as
;;; a hash table.
(defclass hash-namespace (namespace)
  ()
  (:default-initargs :bindings (make-hash-table :test #'eq)))

(defmethod lookup-in-namespace (name (namespace hash-namespace))
  (gethash name (bindings namespace)))

(defmethod add-binding (name value (namespace hash-namespace))
  (setf (gethash name (bindings namespace)) value))

(defclass environment (compiler-object)
  ((%parent :initform nil :initarg :parent :reader parent)))

(defclass global-environment (environment) ())

;;; This global environment does not explicitly contain
;;; any bindings.  Instead we consult the host global
;;; environment when we need to look up bindings. 
(defclass host-global-environment (global-environment) ())

;;; When we compile for a system other than the host,
;;; this global environment should contain all the
;;; information we need about the target system. 
(defclass target-global-environment (global-environment) ())

(defclass lexical-environment (environment)
  ((%variables :initarg :variables
	       :initform (make-instance 'alist-namespace)
	       :reader variables)
   (%functions :initarg :functions
	       :initform (make-instance 'alist-namespace)
	       :reader functions)
   (%macros :initarg :macros
	    :initform (make-instance 'alist-namespace)
	    :reader macros)
   (%block-tags :initarg :block-tags
		:initform (make-instance 'alist-namespace)
		:reader block-tags)
   (%go-tags :initarg :go-tags
	     :initform (make-instance 'alist-namespace)
	     :reader go-tags)
   (%symbol-macros :initarg :symbol-macros
		   :initform (make-instance 'alist-namespace)
		   :reader symbol-macros)))

(defgeneric lookup-level (name environment namespace-name))

(defun lookup (name environment namespace-name)
  (loop for env = environment then (parent env)
	while (not (null env))
	when (lookup-level name env namespace-name)
	  return it))

(defmethod lookup-level (name
			 (environment host-global-environment)
			 (namespace-name (eql 'function)))
  (fdefinition name))

(defmethod lookup-level (name
			 (environment host-global-environment)
			 (namespace-name (eql 'macro)))
  (macro-function name))

(defmethod lookup-level (name
			 (environment host-global-environment)
			 (namespace-name (eql 'special-operator)))
  (special-operator-p name))

(defmethod lookup-level (name
			 (environment host-global-environment)
			 (namespace-name (eql 'symbol-macro)))
  nil)

(defmethod lookup-level (name
			 (environment host-global-environment)
			 (namespace-name (eql 'variable)))
  nil)

(defmethod lookup-level (name
			 environment
			 (namespace-name (eql 'variable)))
  (lookup-in-namespace name (variables environment)))

(defmethod lookup-level (name
			 environment
			 (namespace-name (eql 'macro)))
  (lookup-in-namespace name (macros environment)))
  
(defmethod lookup-level (name
			 environment
			 (namespace-name (eql 'symbol-macro)))
  (lookup-in-namespace name (symbol-macros environment)))
  
(defmethod lookup-level (name
			 (environment lexical-environment)
			 (namespace-name (eql 'special-operator)))
  nil)
  
(defun proper-list-p (list)
  (null (last list 0)))

;;; Create an abstract syntax tree from the raw form returned by
;;; the reader
(defun make-ast (form)
  (cond ((symbolp form)
	 (make-instance 'symbol-form
			:original-form form))
	((atom form)
	 (make-instance 'constant-form
			:original-form form))
	(t
	 (assert (proper-list-p form))
	 (make-instance 'compound-form
			:original-form form
			:subforms (mapcar #'make-ast form)))))

(defgeneric convert (ast environment))

(defmethod convert ((ast constant-form) environment)
  ast)

(defmethod convert ((ast symbol-form) environment)
  (let ((macro-expansion (lookup (original-form ast) environment 'symbol-macro)))
    (if (not (null macro-expansion))
	(convert (make-ast macro-expansion) environment)
	(let ((variable (lookup (original-form ast) environment 'variable)))
	  (if (not (null variable))
	      variable
	      ;; handle unknown variable
	      nil)))))

;;; Search an ast to see whether its original form is eq to the ast
(defun search-ast (form ast)
  (cond ((eq form (original-form ast))
	 ast)
	((typep ast 'compound-form)
	 (some (lambda (subform)
		 (search-ast form subform))
	       (subforms ast)))
	(t nil)))

;;; Create a new ast from the form by either finding
;;; an existing (sub) ast that has form as its original form,
;;; or by creating a new ast. 
(defun fixup (form ast)
  (or (search-ast form ast)
      (cond ((symbolp form)
	     (make-instance 'symbol-form
			    :original-form form))
	    ((atom form)
	     (make-instance 'constant-form
			    :original-form form))
	    (t
	     (assert (proper-list-p form))
	     (make-instance 'compound-form
			    :original-form form
			    :subforms (mapcar (lambda (subform)
						(fixup subform ast))
					      form))))))

(defclass variable-form (form)
  ((%name :initarg :name :reader name)))

(defclass function-call-form (form)
  ((%function-form :initarg :function-form :reader function-form)
   (%arguments :initarg :arguments :reader arguments)))

(defclass progn-form (form)
  ((%body-forms :initarg :body-forms :reader body-forms)))

(defclass binding-form (form)
  ((%variable :initarg :variable :reader variable)
   (%value :initarg :value :reader value)))

(defclass let-form (form)
  ((%bindings :initarg :bindings :reader bindings)
   (%body-form :initarg :body-form :reader body-form)))

(defclass setq-form (form)
  ((%variable :initarg :variable :reader variable)
   (%value :initarg :value :reader value)))

(defclass if-form (form)
  ((%test-form :initarg :test-form :reader test-form)
   (%then-form :initarg :then-form :reader then-form)
   (%else-form :initarg :else-form :reader else-form)))

(defgeneric convert-special (operator ast environment))

(defmethod convert ((ast compound-form) environment)
  ;; possibly expand macros
  (let ((macro-function
	 (lookup (car (original-form ast)) environment 'macro)))
    (if (not (null macro-function))
	(convert (fixup (funcall macro-function (original-form ast) nil) ast) environment)
	(let ((special-operator
	       (lookup (car (original-form ast)) environment 'special-operator)))
	  (if (not (null special-operator))
	      (convert-special (car (original-form ast))
			       ast
			       environment)
	      (if (symbolp (car (original-form ast)))
		  (make-instance 'function-call-form
				 :original-form (original-form ast)
				 ;; this is wrong of course
				 :function-form (car (original-form ast))
				 :arguments
				 (mapcar (lambda (argument)
					   (convert argument environment))
					 (cdr (subforms ast))))
		  ;; this is wrong too
		  nil))))))
	      
(defmethod convert-special ((op (eql 'setq)) ast environment)
  (assert (= (length (subforms ast)) 3))
  (assert (typep (car (subforms ast)) 'symbol-form))
  ;; FIXME: issue a warning if the variable doesn't exist 
  (make-instance 'setq-form
		 :original-form (original-form ast)
		 :variable (lookup (original-form (cadr (subforms ast)))
				   environment
				   'variable)
		 :value (convert (caddr (subforms ast)) environment)))

(defmethod convert-special ((op (eql 'progn)) ast environment)
  (cond ((null (cdr (subforms ast)))
	 (make-instance 'constant-form
			:original-form nil
			:value nil))
	((null (cddr (subforms ast)))
	 (convert (cadr (subforms ast)) environment))
	(t (make-instance 'progn-form
	     :original-form (original-form ast)
	     :body-forms (loop for form in (cdr (subforms ast))
			       collect (convert form environment))))))

(defun convert-implicit-progn (asts environment)
  (let ((ast (make-instance 'compound-form
			    :original-form nil
			    :subforms
			    (cons (make-instance 'symbol-form
						 :original-form nil)
				  asts))))
    (convert-special 'progn ast environment)))

(defmethod convert-special ((op (eql 'let)) ast environment)
  (assert (>= (length (subforms ast)) 2))
  (destructuring-bind (binding-forms &rest body-forms) (cdr (subforms ast))
    (assert (typep binding-forms 'compound-form))
    (let* ((new-environment (make-instance 'lexical-environment
					   :parent environment))
	   (bindings
	    (loop for binding-form in (subforms binding-forms)
		  do (assert (or (typep binding-form 'symbol-form)
				 (and (typep binding-form 'compound-form)
				      (= (length (subforms binding-form)) 2)
				      (typep (car (subforms binding-form))
					     'symbol-form))))
		  collect (make-instance 'binding-form
			    :original-form (original-form binding-form)
			    :variable 
			    (make-instance 'variable-form
			      :original-form 
			      (original-form (if (typep binding-form 'symbol-form)
						 binding-form
						 (car (subforms binding-form))))
			      :name
			      (original-form (if (typep binding-form 'symbol-form)
						 binding-form
						 (car (subforms binding-form)))))
			    :value 
			    (convert (if (typep binding-form 'symbol-form)
					 nil
					 (cadr (subforms binding-form)))
				     environment)))))
      (loop for binding in bindings
	    do (add-binding (name (variable binding))
			    (variable binding)
			    (variables new-environment)))
      (make-instance 'let-form
		     :original-form (original-form ast)
		     :bindings bindings
		     :body-form
		     (convert-implicit-progn body-forms new-environment)))))

(defmethod convert-special ((op (eql 'quote)) ast environment)
  (assert (= (length (subforms ast)) 2))
  (make-instance 'constant-form
		 :original-form (original-form ast)
		 :value (cadr (subforms ast))))

(defmethod convert-special ((op (eql 'if)) ast environment)
  (assert (<= 3 (length (subforms ast)) 4))
  (make-instance 'if-form
		 :original-form (original-form ast)
		 :test-form (convert (cadr (subforms ast)) environment)
		 :then-form (convert (caddr (subforms ast)) environment)
		 :else-form (if (null (cdddr (subforms ast)))
				(make-instance 'constant-form
					       :original-form nil
					       :value nil)
				(convert (cadddr (subforms ast)) environment))))

;; (defgeneric normalize (form))

;; (defmethod normalize (form)
;;   form)

;; (defun bindings-or-nil (form)
;;   (if (typep form 'let-form) (bindings form) '()))

;; (defun body-form-or-form (form)
;;   (if (typep form 'let-form) (body-form form) form))

;; (defun mappend (fun &rest lists)
;;   (mapcan #'copy-list
;; 	  (apply #'mapcar fun lists)))

;; (defmethod normalize ((form if-form))
;;   (let ((normalized-test-form (normalize (test-form form)))
;; 	(normalized-then-form (normalize (then-form form)))
;; 	(normalized-else-form (normalize (else-form form))))
;;     (if (and (not (typep normalized-test-form 'let-form))
;; 	     (not (typep normalized-then-form 'let-form))
;; 	     (not (typep normalized-else-form 'let-form)))
;; 	(make-instance 'if-form
;; 	  :test-form normalized-test-form
;; 	  :then-form normalized-then-form
;; 	  :else-form normalized-else-form)
;; 	(make-instance 'let-form
;; 	  :bindings (append (bindings-or-nil normalized-test-form)
;; 			    (bindings-or-nil normalized-then-form)
;; 			    (bindings-or-nil normalized-else-form))
;; 	  :body-form (make-instance 'if-form
;; 		       :test-form (body-form-or-form normalized-test-form)
;; 		       :then-form (body-form-or-form normalized-then-form)
;; 		       :else-form (body-form-or-form normalized-else-form))))))

;; (defun body-forms-or-list-of-form (form)
;;   (if (typep form 'progn-form) (body-forms form) (list form)))

;; ;; (defmethod normalize ((form progn-form))
;; ;;   (let* ((normalized-body-forms (mapcar #'normalize (body-forms form)))
;; ;; 	 (bindings (mappend #'bindings-or-nil normalized-body-forms)))
;; ;;     (if (null bindings)
;; ;; 	(make-instance 'progn-form
;; ;; 	  :body-forms normalized-body-forms)
;; ;; 	(let ((let-free-body-forms (mapcar #'body-form-or-form normalized-body-forms))
;; ;; 	      (new-body-forms (mappend #'body-forms-or-list-form let-free-body-form)))
;; ;; 	  (make-instance 'let-form
;; ;; 	    :bindings bindings
;; ;; 	    :body-form (make-instance 'progn-form
;; ;; 			 :body-forms new-body-forms))))))

;; (defmethod normalize ((form let-form))
;;   (let ((normalized-body-form (normalize (body-form form))))
;;     (if (typep normalized-body-form 'let-form)
;; 	(make-instance 'let-form
;; 	  :bindings (append (bindings form) (bindings normalized-body-form))
;; 	  :body-form (body-form normalized-body-form))
;; 	(make-instance 'let-form
;; 	  :bindings (bindings form)
;; 	  :body-form normalized-body-form))))

;; ;; (defmethod normalize ((form function-call-form))
;; ;;   (let ((function-reference (function-reference form))
;; ;; 	(normalized-arguments (mapcar #'normalize (arguments form)))
;; ;; 	(bindings (mappend #'bindings-or-nil normalized-arguments)))
;; ;;     (if (null bindings)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;
;; ;;; Graph

;; (defclass instruction () ())

;; (defclass single-successor-instruction (instruction)
;;   ((%successor :initarg :successor :accessor successor)))

;; (defclass copy-instruction (single-successor-instruction)
;;   ((%destination :initarg :destination :accessor destination)
;;    (%source :initarg :source :accessor source)))

;; (defclass load-instruction (single-successor-instruction)
;;   ((%destination :initarg :destination :accessor destination)
;;    (%value :initarg :value :accessor value)))

;; (defclass test-instruction (instruction)
;;   ((%place :initarg :place :accessor place)
;;    (%true-successor :initarg :true-successor :accessor true-successor)
;;    (%false-successor :initarg :false-successor :accessor false-successor)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;
;; ;;; Compile an expression in internal form

;; (defgeneric compile-expression (form values successors))

;; ;;; factor the duplicated code
;; (defmethod compile-expression ((form constant-form) values successors)
;;   (if (null values)
;;       (warn "constant form compiled in a context where its value is not needed")
;;       (let ((succ successors))
;; 	(loop for place in (cdr values)
;; 	      do (setf succ (make-instance 'load-instruction
;; 			      :destination place
;; 			      :value nil
;; 			      :successor succ)))
;; 	(make-instance 'load-instruction
;; 	  :destination (car values)
;; 	  :value form
;; 	  :successor succ))))

;; ;;; factor the duplicated code
;; (defmethod compile-expression ((form variable-form) values successors)
;;   (if (null values)
;;       (warn "variable form compiled in a context where its value is not needed")
;;       (let ((succ successors))
;; 	(loop for place in (cdr values)
;; 	      do (setf succ (make-instance 'load-instruction
;; 			      :destination place
;; 			      :value nil
;; 			      :successor succ)))
;; 	(make-instance 'copy-instruction
;; 	  :destination (car values)
;; 	  :value form
;; 	  :successor succ))))
