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
;;; The base classes for conditions used here. 

(define-condition compilation-program-error (program-error)
  ;; Add information about where to locate the form.
  ((%form :initarg :form :reader form)))

(define-condition compilation-warning (warning)
  ;; Add information about where to locate the form.
  ((%form :initarg :form :reader form)))

(define-condition compilation-style-warning (style-warning)
  ;; Add information about where to locate the form.
  ((%form :initarg :form :reader form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Internal representation of source code

;;; Terminology: We use `form' to mean raw Lisp expressions
;;; resulting from READing source code.  We use `ast'  for
;;; the abstract syntax tree resulting from converting such
;;; raw expressions to instances of our compiler objects. 

;;; The first step is to turn raw forms into abstract syntax trees
;;; (asts), so that we can then use generic function dispatch for the
;;; following steps. .  In this step, we only distinguish between 3
;;; kinds of ast, a symbol, a non-symbol atom, and a compound ast.

(defclass ast (compiler-object)
  ((%form :initarg :form :accessor form)))

(defclass symbol-ast (ast) ())  

(defclass constant-ast (ast) ())

(defclass compound-ast (ast)
  ((%children :initarg :children :reader children)))

(defun proper-list-p (list)
  (null (last list 0)))

(define-condition form-is-not-a-proper-list (compilation-program-error) ())

(defun make-ast (form)
  (cond ((symbolp form)
	 (make-instance 'symbol-ast
			:form form))
	((atom form)
	 (make-instance 'constant-ast
			:form form))
	(t
	 (unless (proper-list-p form)
	   (error 'form-is-not-a-proper-list :form form))
	 (make-instance 'compound-ast
			:form form
			:children (mapcar #'make-ast form)))))

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
  
;;; The second step is to turn the trivial ast into something more
;;; sophisiticated, in which the environment has been consulted to
;;; determine whether we have a special-form, a function call, a macro
;;; call, etc.  In this step, we also expand macros, so that the 
;;; resulting ast only contains special forms and function calls.  We
;;; also recognize every special form and take into account the influence
;;; on the environment of each one.  So for instance, a LET results in
;;; the creation of a new environment which is used to convert the
;;; body of the LET. 
(defgeneric convert (ast environment))

(defmethod convert ((ast constant-ast) environment)
  ast)

(defmethod convert ((ast symbol-ast) environment)
  (let ((macro-expansion (lookup (form ast) environment 'symbol-macro)))
    (if (not (null macro-expansion))
	(convert (make-ast macro-expansion) environment)
	(let ((variable (lookup (form ast) environment 'variable)))
	  (if (not (null variable))
	      variable
	      ;; handle unknown variable
	      nil)))))

;;; Create a new ast from the form by either finding an existing (sub)
;;; ast that has form as its original form, or by creating a new ast.
;;; This could be done a lot better.  For one thing, it is going to do
;;; the wrong thing if the original form contains a symbol, that also
;;; happens to be introduced by the macro.  The same thing is true for
;;; things like small numbers, characters, etc.  One possibility would
;;; be to try to expand the macro with some unique object in place of
;;; suspicious atoms, and see whether the expansion then contains
;;; those unique objects.  If it does, then we are sure of the origin,
;;; and if it does not contain it, then we know it was introduced by
;;; the macro expander.  However, that technique might fail in a
;;; couple of ways.  First, the macro function might have inspected
;;; the object and failed to expand, so we have to catch such errors.
;;; Luckily, we are allowed to call the macroexpander as many times as
;;; we like.  Also, the macro function might have done silly things
;;; such as applied COPY-TREE to some parts of the original form. 
;;;
;;; So here is a suggested technique.  Initially, recognize only
;;; objects that cannot have been introduced by the macro, such as
;;; (typically) conses, but also atoms other than numbers, symbols,
;;; and characters.  In a second step, check the expanded form for
;;; numbers, symbols, and characters that do not exist in the original
;;; form.  Such atoms must have been introduced by the macroexpander,
;;; and a new ast can be created form them.  In a third step, Check
;;; the expanded form for atoms that are not inside already recognized
;;; subforms.  For each such atom in the original form, replace it by
;;; a different object of the same type and macroexpand again.  If the
;;; macroexpansion succeeds, then identify the original ast.  So for
;;; instance, if we suspect that a symbol from the original form shows
;;; up in the expanded form, replace it by a GENSYMed symbol and try
;;; again.  If a number shows up, replace it by (1+ number) and try
;;; again.  If a character shows up, replace it by the one with the
;;; next code, and try again. 

;;; Search an ast to see whether its original form is eq to the ast
(defun search-ast (form ast)
  (cond ((eq form (form ast))
	 ast)
	((typep ast 'compound-ast)
	 (some (lambda (child)
		 (search-ast form child))
	       (children ast)))
	(t nil)))

(defun fixup (form ast)
  (or (search-ast form ast)
      (cond ((symbolp form)
	     (make-instance 'symbol-ast
			    :form form))
	    ((atom form)
	     (make-instance 'constant-ast
			    :form form))
	    (t
	     (assert (proper-list-p form))
	     (make-instance 'compound-ast
			    :form form
			    :children (mapcar (lambda (child)
						(fixup child ast))
					      form))))))

(defclass variable-ast (ast)
  ((%name :initarg :name :reader name)))

(defclass function-call-ast (ast)
  ((%function-ast :initarg :function-ast :reader function-ast)
   (%arguments :initarg :arguments :reader arguments)))

(defclass progn-ast (ast)
  ((%body-asts :initarg :body-asts :reader body-asts)))

(defclass binding-ast (ast)
  ((%variable :initarg :variable :reader variable)
   (%value :initarg :value :reader value)))

(defclass let-ast (ast)
  ((%bindings :initarg :bindings :reader bindings)
   (%body-ast :initarg :body-ast :reader body-ast)))

(defclass setq-ast (ast)
  ((%variable :initarg :variable :reader variable)
   (%value :initarg :value :reader value)))

(defclass if-ast (ast)
  ((%test-ast :initarg :test-ast :reader test-ast)
   (%then-ast :initarg :then-ast :reader then-ast)
   (%else-ast :initarg :else-ast :reader else-ast)))

(defgeneric convert-special (operator ast environment))

(defmethod convert ((ast compound-ast) environment)
  ;; possibly expand macros
  (let ((macro-function
	 (lookup (car (form ast)) environment 'macro)))
    (if (not (null macro-function))
	(convert (fixup (funcall macro-function (form ast) nil) ast) environment)
	(let ((special-operator
	       (lookup (car (form ast)) environment 'special-operator)))
	  (if (not (null special-operator))
	      (convert-special (car (form ast))
			       ast
			       environment)
	      (if (symbolp (car (form ast)))
		  (make-instance 'function-call-ast
				 :form (form ast)
				 ;; this is wrong of course
				 :function-ast (car (form ast))
				 :arguments
				 (mapcar (lambda (argument)
					   (convert argument environment))
					 (cdr (children ast))))
		  ;; this is wrong too
		  nil))))))
	      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting setq

(define-condition setq-must-have-even-number-of-arguments
    (compilation-program-error)
  ())

(define-condition setq-variable-must-be-a-symbol
    (compilation-program-error)
  ())

(define-condition setq-variable-does-not-exist
    (compilation-warning)
  ())

(defmethod convert-special ((op (eql 'setq)) ast environment)
  ;; FIXME check if the length is odd
  (assert (= (length (children ast)) 3))
  ;; FIXME check ever other argument
  (assert (typep (car (children ast)) 'symbol-ast))
  ;; FIXME: issue a warning if the variable doesn't exist 
  ;; FIXME: turn it into a progn of setqs with two args. 
  (make-instance 'setq-ast
		 :form (form ast)
		 :variable (lookup (form (cadr (children ast)))
				   environment
				   'variable)
		 :value (convert (caddr (children ast)) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting progn

(defmethod convert-special ((op (eql 'progn)) ast environment)
  (cond ((null (cdr (children ast)))
	 (make-instance 'constant-ast
			:form nil
			:value nil))
	((null (cddr (children ast)))
	 (convert (cadr (children ast)) environment))
	(t (make-instance 'progn-ast
	     :form (form ast)
	     :body-asts (loop for ast in (cdr (children ast))
			       collect (convert ast environment))))))

(defun convert-implicit-progn (asts environment)
  (let ((ast (make-instance 'compound-ast
			    :form nil
			    :children
			    (cons (make-instance 'symbol-ast
						 :form nil)
				  asts))))
    (convert-special 'progn ast environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting let

(defmethod convert-special ((op (eql 'let)) ast environment)
  (assert (>= (length (children ast)) 2))
  (destructuring-bind (binding-asts &rest body-asts) (cdr (children ast))
    (assert (typep binding-asts 'compound-ast))
    (let* ((new-environment (make-instance 'lexical-environment
					   :parent environment))
	   (bindings
	    (loop for binding-ast in (children binding-asts)
		  do (assert (or (typep binding-ast 'symbol-ast)
				 (and (typep binding-ast 'compound-ast)
				      (= (length (children binding-ast)) 2)
				      (typep (car (children binding-ast))
					     'symbol-ast))))
		  collect (make-instance 'binding-ast
			    :form (form binding-ast)
			    :variable 
			    (make-instance 'variable-ast
			      :form 
			      (form (if (typep binding-ast 'symbol-ast)
						 binding-ast
						 (car (children binding-ast))))
			      :name
			      (form (if (typep binding-ast 'symbol-ast)
						 binding-ast
						 (car (children binding-ast)))))
			    :value 
			    (convert (if (typep binding-ast 'symbol-ast)
					 nil
					 (cadr (children binding-ast)))
				     environment)))))
      (loop for binding in bindings
	    do (add-binding (name (variable binding))
			    (variable binding)
			    (variables new-environment)))
      (make-instance 'let-ast
		     :form (form ast)
		     :bindings bindings
		     :body-ast
		     (convert-implicit-progn body-asts new-environment)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting quote

(defmethod convert-special ((op (eql 'quote)) ast environment)
  (assert (= (length (children ast)) 2))
  (make-instance 'constant-ast
		 :form (form ast)
		 :value (cadr (children ast))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting if

(defmethod convert-special ((op (eql 'if)) ast environment)
  (assert (<= 3 (length (children ast)) 4))
  (make-instance 'if-ast
		 :form (form ast)
		 :test-ast (convert (cadr (children ast)) environment)
		 :then-ast (convert (caddr (children ast)) environment)
		 :else-ast (if (null (cdddr (children ast)))
			       (make-instance 'constant-ast
					      :form nil
					      :value nil)
			       (convert (cadddr (children ast)) environment))))

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
;; (defmethod compile-expression ((form constant-ast) values successors)
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
