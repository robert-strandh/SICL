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

(define-condition form-is-must-be-a-proper-list (compilation-program-error) ())

(defun make-ast (form)
  (cond ((symbolp form)
	 (make-instance 'symbol-ast
			:form form))
	((atom form)
	 (make-instance 'constant-ast
			:form form))
	(t
	 (unless (proper-list-p form)
	   (error 'form-must-be-a-proper-list :form form))
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
	     (unless (proper-list-p form)
	       (error 'form-must-be-a-proper-list
		      :form form))
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
;;; Converting progn

(defclass progn-ast (ast)
  ((%body-asts :initarg :body-asts :reader body-asts)))

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

;;; FIXME: generate a form by consing PROGN to the front???
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
;;; Converting block

(defclass block-ast (ast)
  ((%tag :initarg :tag :reader tag)
   (%body-ast :initarg :body-ast :reader body-ast)))

(define-condition block-must-have-at-least-one-argument
    (compilation-program-error)
  ())

(define-condition block-name-must-be-a-symbol
    (compilation-program-error)
  ())

(defmethod convert-special ((op (eql 'block)) ast environment)
  ;; Check that there is at least one argument.
  (unless (>= (length (children ast)) 2)
    (error 'block-must-have-at-least-one-argument
	   :form (form ast)))
  ;; Check that the block name is a symbol
  (unless (typep (cadr (children ast)) 'symbol-ast)
    (error 'block-name-must-be-a-symbol
	   :form (cadr (children ast))))
  (let ((new-environment (make-instance 'lexical-environment
					:parent environment)))
    (add-binding (form (cadr (children ast)))
		 ;; Do we want to introduce a class for block tags?
		 (cadr (children ast)) 
		 (block-tags new-environment))
    (make-instance 'block-ast
      :form (form ast)
      :tag (cadr (children ast))
      :body-ast (convert-implicit-progn (cddr (children ast)) new-environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting return-from

(defclass return-from-ast (ast)
  ((%tag :initarg :tag :reader tag)
   (%result :initarg :result :reader result)))

(define-condition return-from-must-have-one-or-two-arguments
    (compilation-program-error)
  ())

(defmethod convert-special ((op (eql 'return-from)) ast environment)
  ;; Check that we have the right number of arguments
  (unless (<= 2 (length (children ast)) 3)
    (error 'return-from-must-have-one-or-two-arguments
	   :form (form ast)))
  (make-instance 'return-from-ast
    :form (form ast)
    :tag (lookup (form (cadr ast)) environment 'block-tag)
    :result (if (= (length (children ast)) 3)
		(convert (caddr (children ast)) environment)
		(make-instance 'constant-ast :form nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting setq

(defclass setq-ast (ast)
  ((%variable :initarg :variable :reader variable)
   (%value :initarg :value :reader value)))

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
  ;; Check that the progn form has an even number of arguments.
  (unless (oddp (length (children ast)))
    (error 'setq-must-have-even-number-of-arguments
	   :form (form ast)))
  ;; Check that even arguments are symbols. 
  (loop for child in (cdr (children ast)) by #'cddr
	do (unless (typep child 'symbol-ast)
	     (error 'setq-variable-must-be-a-symbol
		    :form (form child))))
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
;;; Converting let

(defclass binding-ast (ast)
  ((%variable :initarg :variable :reader variable)
   (%value :initarg :value :reader value)))

(defclass let-ast (ast)
  ((%bindings :initarg :bindings :reader bindings)
   (%body-ast :initarg :body-ast :reader body-ast)))

(define-condition bindings-must-be-a-list
    (compilation-program-error)
  ())

(define-condition binding-must-be-symbol-or-list
    (compilation-program-error)
  ())

(define-condition binding-must-have-length-two
    (compilation-program-error)
  ())

(define-condition variable-must-be-a-symbol
    (compilation-program-error)
  ())    

(defun check-bindings (binding-asts)
  (unless (typep binding-asts 'compound-ast)
    (error 'bindings-must-be-a-list :form (form binding-asts)))
  (loop for binding-ast in (children binding-asts)
	do (unless (or (typep binding-ast 'symbol-ast)
		       (typep binding-ast 'compound-ast))
	     (error 'binding-must-be-symbol-or-list
		    :form (form binding-ast)))
	   (when (and (typep binding-ast 'compound-ast)
		      (/= (length (children binding-ast)) 2))
	     (error 'binding-must-have-length-two
		    :form (form binding-ast)))
	   (when (and (typep binding-ast 'compound-ast)
		      (= (length (children binding-ast)) 2)
		      (not (typep (car (children binding-ast))
				  'symbol-ast)))
	     (error 'variable-must-be-a-symbol
		    :form (form (car (children binding-ast)))))))

(define-condition form-must-have-bindings
    (compilation-program-error)
  ())

;;; FIXME: handle declarations
(defmethod convert-special ((op (eql 'let)) ast environment)
  (unless (>= (length (children ast)) 2)
    (error 'must-have-bindings :form (form ast)))
  (destructuring-bind (binding-asts &rest body-asts) (cdr (children ast))
    (check-bindings binding-asts)
    (let* ((new-environment (make-instance 'lexical-environment
					   :parent environment))
	   (bindings
	    (loop for binding-ast in (children binding-asts)
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

(define-condition quote-must-have-a-single-argument
    (compilation-program-error)
  ())

(defmethod convert-special ((op (eql 'quote)) ast environment)
  (unless (= (length (children ast)) 2)
    (error 'quote-must-have-a-single-argument
	   :form (form ast)))
  (make-instance 'constant-ast
		 :form (form ast)
		 :value (cadr (children ast))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting if

(defclass if-ast (ast)
  ((%test-ast :initarg :test-ast :reader test-ast)
   (%then-ast :initarg :then-ast :reader then-ast)
   (%else-ast :initarg :else-ast :reader else-ast)))

(define-condition if-must-have-three-or-four-arguments
    (compilation-program-error)
  ())

(defmethod convert-special ((op (eql 'if)) ast environment)
  (unless (<= 3 (length (children ast)) 4)
    (error 'if-must-have-three-or-four-arguments
	   :form (form ast)))
  (make-instance 'if-ast
		 :form (form ast)
		 :test-ast (convert (cadr (children ast)) environment)
		 :then-ast (convert (caddr (children ast)) environment)
		 :else-ast (if (null (cdddr (children ast)))
			       (make-instance 'constant-ast
					      :form nil
					      :value nil)
			       (convert (cadddr (children ast)) environment))))

