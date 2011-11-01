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

;;; Terminology: We use `form' to mean raw Lisp expressions
;;; resulting from READing source code.  We use `ast'  for
;;; the abstract syntax tree resulting from converting such
;;; raw expressions to instances of our compiler objects, or
;;; from using a sophisticated reader that associate source
;;; code position with forms. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The base classes for conditions used here. 

;;; These classes have a slot that will contain an abstract
;;; syntax tree.  A standard report function would just
;;; show the corresponding form, but condition handler 
;;; used in an integrated development environment could
;;; determine the location of the source code from the 
;;; ast, load the corresponding file and highlight the
;;; relevant part. 

(define-condition compilation-program-error (program-error)
  ((%ast :initarg :ast :reader ast)))

(define-condition compilation-warning (warning)
  ((%ast :initarg :ast :reader ast)))

(define-condition compilation-style-warning (style-warning)
  ((%ast :initarg :ast :reader ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Internal representation of source code

;;; The first step is to turn raw forms into abstract syntax trees
;;; (asts), so that we can then use generic function dispatch for the
;;; following steps. .  In this step, we only distinguish between 3
;;; kinds of ast, a symbol, a non-symbol atom, and a compound ast.
;;; Eventually, we will have a reader that delivers asts directly, and
;;; they will be labeled with the location of the source code. 

(defclass ast (compiler-object)
  ((%form :initarg :form :accessor form)))

(defclass symbol-ast (ast) ())  

(defclass constant-ast (ast) ())

(defclass compound-ast (ast)
  ((%children :initarg :children :reader children)))

;;; Like mapcar, but if the list is a dotted list, 
;;; preserve that structure and apply the function to
;;; the terminating atom as well. 
(defun map-maybe-dotted-list (fun list)
  (cond ((null list)
	 nil)
	((atom list)
	 (funcall fun list))
	(t
	 (cons (funcall fun (car list))
	       (map-maybe-dotted-list fun (cdr list))))))

(defun make-ast (form)
  (cond ((symbolp form)
	 (make-instance 'symbol-ast :form form))
	((atom form)
	 (make-instance 'constant-ast :form form))
	(t
	 (let ((children (map-maybe-dotted-list #'make-ast form)))
	   (make-instance 'compound-ast
			  :children children
			  :form (map-maybe-dotted-list #'form children))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Environment
;;;
;;; This code is currently wrong, because it assumes that
;;; every failed lookup in a recent partial environment should test
;;; the next environment up the chain, but that isn't true, since
;;; for instance local functions shadow macros and compiler macros. 
;;; The way to fix this is to eliminate the lookup-level function, 
;;; and have the lookup function decide, for each namespace, whether
;;; to look further up the chain. 

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

(defmethod lookup-level (name
			 (environment lexical-environment)
			 (namespace-name (eql 'go-tag)))
  (lookup-in-namespace name (go-tags environment)))
  
(defmethod lookup-level (name
			 (environment lexical-environment)
			 (namespace-name (eql 'block-tag)))
  (lookup-in-namespace name (block-tags environment)))
  
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
	     (make-instance 'compound-ast
			    :form form
			    :children
			    (map-maybe-dotted-list (lambda (child)
						     (fixup child ast))
						   form))))))

(define-condition form-must-be-a-proper-list (compilation-program-error) ())

(defun proper-list-p (list)
  (null (last list 0)))

(defclass variable-ast (ast)
  ((%name :initarg :name :reader name)))

(defclass function-call-ast (ast)
  ((%function-ast :initarg :function-ast :reader function-ast)
   (%arguments :initarg :arguments :reader arguments)))

(defgeneric convert-special (operator ast environment))

(defmethod convert ((ast compound-ast) environment)
  ;; I seem to have read somewhere that all valid compound
  ;; forms are proper lists, but I am not entirely sure about it. 
  (unless (proper-list-p (form ast))
    (error 'form-must-be-a-proper-list
	   :ast ast))
  ;; possibly expand macros
  ;; FIXME: all this should be done by our own version of 
  ;; macroexpand.  It has to be our own version because 
  ;; macroexpand-1 is the one that uses the environment to
  ;; find the correct macro function to use, and the host
  ;; cannot understand the environment structure of the target. 
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
	   :ast ast))
  ;; Check that the block name is a symbol
  (unless (typep (cadr (children ast)) 'symbol-ast)
    (error 'block-name-must-be-a-symbol
	   :ast (cadr (children ast))))
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
	   :ast ast))
  (make-instance 'return-from-ast
    :form (form ast)
    :tag (lookup (form (cadr (children ast))) environment 'block-tag)
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
	   :ast ast))
  ;; Check that even arguments are symbols. 
  (loop for child in (cdr (children ast)) by #'cddr
	do (unless (typep child 'symbol-ast)
	     (error 'setq-variable-must-be-a-symbol
		    :ast child)))
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
    (error 'bindings-must-be-a-list :ast binding-asts))
  (loop for binding-ast in (children binding-asts)
	do (unless (or (typep binding-ast 'symbol-ast)
		       (typep binding-ast 'compound-ast))
	     (error 'binding-must-be-symbol-or-list
		    :ast binding-ast))
	   (when (and (typep binding-ast 'compound-ast)
		      (/= (length (children binding-ast)) 2))
	     (error 'binding-must-have-length-two
		    :ast binding-ast))
	   (when (and (typep binding-ast 'compound-ast)
		      (= (length (children binding-ast)) 2)
		      (not (typep (car (children binding-ast))
				  'symbol-ast)))
	     (error 'variable-must-be-a-symbol
		    :ast (car (children binding-ast))))))

(define-condition form-must-have-bindings
    (compilation-program-error)
  ())

;;; FIXME: handle declarations
(defmethod convert-special ((op (eql 'let)) ast environment)
  (unless (>= (length (children ast)) 2)
    (error 'must-have-bindings :ast ast))
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
	   :ast ast))
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
	   :ast ast))
  (make-instance 'if-ast
		 :form (form ast)
		 :test-ast (convert (cadr (children ast)) environment)
		 :then-ast (convert (caddr (children ast)) environment)
		 :else-ast (if (null (cdddr (children ast)))
			       (make-instance 'constant-ast
					      :form nil
					      :value nil)
			       (convert (cadddr (children ast)) environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting catch

(defclass catch-ast (ast)
  ((%tag :initarg :tag :reader tag)
   (%body-ast :initarg :body-ast :reader body-ast)))

(define-condition catch-must-have-at-least-one-argument
    (compilation-program-error)
  ())

(defmethod convert-special ((op (eql 'catch)) ast environment)
  ;; Check the number of arguments
  (unless (>= (length (children ast)) 2)
    (error 'catch-must-have-at-least-one-argument
	   :ast ast))
  (make-instance 'catch-ast
    :form (form ast)
    :tag (convert (cadr (children ast)) environment)
    :body-ast (convert-implicit-progn (cddr (children ast)) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting throw

(defclass throw-ast (ast)
  ((%tag :initarg :tag :reader tag)
   (%result :initarg :result :reader result)))

(define-condition throw-must-have-exactly-two-arguments
    (compilation-program-error)
  ())

(defmethod convert-special ((op (eql 'throw)) ast environment)
  ;; Check the number of arguments
  (unless (= (length (children ast)) 3)
    (error 'throw-must-have-exactly-two-arguments
	   :ast ast))
  (make-instance 'throw-ast
    :form (form ast)
    :tag (convert (cadr (children ast)) environment)
    :result (convert (caddr (children ast)) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting tagbody

(defclass tag-ast (ast)
  ((%name :initarg name :reader name)))

(defclass tagbody-ast (ast)
  ((%body-asts :initarg body-asts :reader body-asts)))

(define-condition go-tag-must-be-symbol-or-integer
    (compilation-program-error)
  ())

(defmethod convert-special ((op (eql 'tagbody)) ast environment)
  (let ((new-environment (make-instance 'lexical-environment
					:parent environment)))
    ;; Gather up the tags from the body and insert them into
    ;; the new environment
    ;; FIXME: we might want to check that the tags are unique. 
    (loop for child in (children ast)
	  do (unless (typep child 'compound-ast)
	       (unless (or (typep (form child) 'symbol)
			   (typep (form child) 'integer))
		 (error 'go-tag-must-be-symbol-or-integer
			:ast child))
	       (add-binding (form child)
			    (make-instance 'tag-ast :form (form child))
			    (go-tags new-environment))))
    (make-instance 'tagbody-ast
      :form (form ast)
      :body-asts (loop for child in (children ast)
		       collect (if (typep child 'compound-ast)
				   (convert child new-environment)
				   (lookup (form child) new-environment 'go-tag))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting go

(defclass go-tag-ast (ast)
  ((%tag :initarg :tag :reader tag)))

(define-condition go-must-have-exactly-one-argument
    (compilation-program-error)
  ())

(defmethod convert-special ((op (eql 'go)) ast environment)
  ;; Check the number of arguments
  (unless (= (length (children ast)) 2)
    (error 'go-must-have-exactly-one-argument
	   :ast ast))
  ;; Check that the go tag is a symbol or an integer
  (unless (or (typep (cadr (children ast)) 'integer)
	      (typep (cadr (children ast)) 'symbol))
    (error 'go-tag-must-be-symbol-or-integer
	   :ast (cadr (children ast))))
  (make-instance 'go-tag-ast
		 :form (cadr (children ast))
		 :tag (lookup (cadr (children ast)) environment 'go-tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting eval-when

(defclass eval-when-ast (ast)
  ((%situations :initarg :situations :reader situations)
   (%body-ast :initarg :body-ast :reader body-ast)))

(define-condition eval-when-must-have-at-least-one-argument
    (compilation-program-error)
  ())

(define-condition situations-must-be-a-list
    (compilation-program-error)
  ())

(define-condition invalid-eval-when-situation
    (compilation-program-error)
  ())

(defmethod convert-special ((op (eql 'eval-when)) ast environment)
  ;; Check the number of arguments
  (unless (>= (length (children ast)) 2)
    (error 'eval-when-must-have-at-least-one-argument
	   :ast ast))
  ;; Check that the first argument is a list
  (unless (typep (cadr (children ast)) 'compound-ast)
    (error 'situations-must-be-a-list
	   :ast (cadr (children ast))))
  ;; Check each situation
  (loop for situation in (cadr (children ast))
	do (unless (and (typep situation 'symbol-ast)
			(member (form situation)
				'(:compile-toplevel :load-toplevel :execute
				  compile load eval)))
	     ;; FIXME: perhaps we should warn about the deprecated situations
	     (error 'invalid-eval-when-situation
		    :ast situation)))
  (make-instance 'eval-when-ast
    :form (form ast)
    :situations (mapcar #'form (children (cadr ast)))
    :body-ast (convert-implicit-progn (cddr (children ast)) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting the

(defclass the-ast (ast)
  ((%value-type :initarg :value-type :reader value-type)
   (%result :initarg :result :reader result)))

(define-condition the-must-have-exactly-two-arguments
    (compilation-program-error)
  ())

(defmethod convert-special ((op (eql 'the)) ast environment)
  ;; Check the number of arguments
  (unless (= (length (children ast)) 3)
    (error 'the-must-have-exactly-two-arguments
	   :ast ast))
  (make-instance 'the-ast
    :form (form ast)
    ;; FIXME: do something smarter about the type
    :value-type (cadr (children ast))
    :result (convert (caddr (children ast)) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting unwind-protect

(defclass unwind-protect-ast (ast)
  ((%protected-ast :initarg :protected-ast :reader protected-ast)
   (%cleanup-ast :initarg :cleanup-ast :reader cleanup-ast)))

(define-condition unwind-protect-must-have-at-leat-one-argument
    (compilation-program-error)
  ())

(defmethod convert-special ((op (eql 'unwind-protect)) ast environment)
  ;; Check the number of arguments
  (unless (>= (length (children ast)) 2)
    (error 'unwind-protect-must-have-at-leat-one-argument
	   :ast ast))
  (make-instance 'unwind-protect-ast
    :form (form ast)
    :cleanup-ast (convert-implicit-progn (cddr (children ast)) environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse a lambda list

(define-condition parameter-starting-with-ampersand
    (compilation-warning)
  ())

(define-condition misplaced-optional-in-lambda-list
    (compilation-program-error)
  ())

(define-condition misplaced-rest-in-lambda-list
    (compilation-program-error)
  ())

(define-condition misplaced-key-in-lambda-list
    (compilation-program-error)
  ())

(define-condition misplaced-aux-in-lambda-list
    (compilation-program-error)
  ())

(define-condition key-or-aux-expected
    (compilation-program-error)
  ())

;;; FIXME: introduce style warnings for situations when a
;;; lambda-list keyword taking a list of parameters is
;;; not followed by any parmeter at all. 

(defparameter *lambda-list-keywords* 
  '(&optional &rest &body &key &aux &whole &environment &allow-other-keys))

;;; The different parsers take a list of asts and return two values A
;;; list of parameters (or a single parameter in case of &rest,
;;; &whole, etc)and the remaining lambda list to parse.  The
;;; lambda-list keyword has been removed before the parser is called.
;;; The original-ast is used only for reporting errors and warnings.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser for required parameters

(define-condition malformed-required-parameter
    (compilation-program-error)
  ())

(defun parse-required (asts original-ast)
  (declare (ignore original-ast))
  (loop with result = '()
	until (or (null asts)
		  (member (form (car asts)) *lambda-list-keywords*))
	do (let ((parameter (pop asts)))
	     (unless (symbolp (form parameter))
	       (error 'malformed-required-parameter
		      :ast parameter))
	     (when (eql (char (symbol-name (form parameter)) 0) #\&)
	       (warn 'parameter-starting-with-ampersand
		     :ast parameter))
	     (push parameter result))
	finally (return (values (nreverse result) asts))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser for optional parameters

(define-condition empty-optional-parameter-list
    (compilation-style-warning)
  ())

(define-condition malformed-optional-parameter
    (compilation-program-error)
  ())

(defun parse-optional (asts original-ast)
  (when (or (null asts)
	    (member (form (car asts)) *lambda-list-keywords*))
    (warn 'empty-optional-parameter-list
	  ;; find the &optional lambda-list keyword
	  :ast (car (last (children original-ast) (1+ (length asts))))))
  (loop with result = '()
	until (or (null asts)
		  (member (form (car asts)) *lambda-list-keywords*))
	do (let ((parameter (pop asts)))
	     (cond ((symbolp (form parameter))
		    (when (eql (char (symbol-name (form parameter)) 0) #\&)
		      (warn 'parameter-starting-with-ampersand
			    :ast parameter))
		    (push parameter result))
		   ;; FIXME: check for more problems.
		   ((or (not (consp (form parameter)))
			(not (proper-list-p (form parameter)))
			(> (length (children parameter)) 3))
		    (error 'malformed-optional-parameter
			   :ast parameter))
		   (t
		    (push parameter result))))
	finally (return (values (nreverse result) asts))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser for rest parameter

(define-condition rest-must-be-followed-by-a-prameter
    (compilation-program-error)
  ())

(define-condition malformed-rest-parameter
    (compilation-program-error)
  ())

(defun parse-rest (asts original-ast)
  (when (or (null asts)
	    (member (form (car asts)) *lambda-list-keywords*))
    (warn 'rest-must-be-followed-by-a-prameter
	  ;; find the &rest lambda-list keyword
	  :ast (car (last (children original-ast) (1+ (length asts))))))
  (let ((parameter (pop asts)))
    (unless (symbolp (form parameter))
      (error 'malformed-rest-parameter
	     :ast parameter))
    (when (eql (char (symbol-name (form parameter)) 0) #\&)
      (warn 'parameter-starting-with-ampersand
	    :ast parameter))
    (values parameter asts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser for key parameters

(define-condition empty-key-parameter-list
    (compilation-style-warning)
  ())

(define-condition key-parameter-must-be-a-symbol-or-a-proper-list
    (compilation-program-error)
  ())

(define-condition key-parameter-list-must-have-length-between-one-and-three
    (compilation-program-error)
  ())

(define-condition key-var-must-be-a-symbol-or-a-list-of-length-one-or-two
    (compilation-program-error)
  ())

(define-condition key-supplied-p-parameter-must-be-a-symbol
    (compilation-program-error)
  ())

(defun parse-key (asts original-ast)
  (when (or (null asts)
	    (member (form (car asts)) *lambda-list-keywords*))
    (warn 'empty-key-parameter-list
	  ;; find the &key lambda-list keyword
	  :ast (car (last (children original-ast) (1+ (length asts))))))
  (loop with result = '()
	until (or (null asts)
		  (member (form (car asts)) *lambda-list-keywords*))
	do (let ((parameter (pop asts)))
	     (cond ((symbolp (form parameter))
		    (when (eql (char (symbol-name (form parameter)) 0) #\&)
		      (warn 'parameter-starting-with-ampersand
			    :ast parameter))
		    (push parameter result))
		   ((or (not (consp (form parameter)))
			(not (proper-list-p (form parameter))))
		    (error 'key-parameter-must-be-a-symbol-or-a-proper-list
			   :ast parameter))
		   ((not (<= 1 (length (children parameter)) 3))
		    (error 'key-parameter-list-must-have-length-between-one-and-three
			   :ast parameter))
		   ((not (or (symbolp (car (form parameter)))
			     (and (consp (car (form parameter)))
				  (proper-list-p (car (form parameter)))
				  (<= 1 (length (car (form parameter))) 2))))
		    (error 'key-var-must-be-a-symbol-or-a-list-of-length-one-or-two
			   :ast (car (children parameter))))
		   (t
		    (when (and (= (length (children parameter)) 3)
			       (not (typep (third (children parameter)) 'symbol-ast)))
		      (error 'key-supplied-p-parameter-must-be-a-symbol
			     :ast (third (children parameter))))
		    (push parameter result))))
	finally (return (values (nreverse result) asts))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser for aux parameters

(define-condition empty-aux-parameter-list
    (compilation-style-warning)
  ())

(define-condition malformed-aux-parameter
    (compilation-program-error)
  ())

(defun parse-aux (asts original-ast)
  (when (or (null asts)
	    (member (form (car asts)) *lambda-list-keywords*))
    (warn 'empty-aux-parameter-list
	  ;; find the &aux lambda-list keyword
	  :ast (car (last (children original-ast) (1+ (length asts))))))
  (loop with result = '()
	until (or (null asts)
		  (member (form (car asts)) *lambda-list-keywords*))
	do (let ((parameter (pop asts)))
	     (cond ((symbolp (form parameter))
		    (when (eql (char (symbol-name (form parameter)) 0) #\&)
		      (warn 'parameter-starting-with-ampersand
			    :ast parameter))
		    (push parameter result))
		   ;; FIXME: check for more problems.
		   ((or (not (consp (form parameter)))
			(not (proper-list-p (form parameter)))
			(> (length (children parameter)) 2))
		    (error 'malformed-aux-parameter
			   :ast parameter))
		   (t
		    (push parameter result))))
	finally (return (values (nreverse result) asts))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse ordinary lambda list

;;; FIXME: factor this later. 
(defclass ordinary-lambda-list ()
  (;; A list of required parameters.
   (%required-parameters :initform '()
			 :initarg :required-parameters
			 :reader required-parameters)
   ;; A list of optional parameters.
   (%optional-parameters :initform '()
			 :initarg :optional-parameters
			 :reader optional-parameters)
   ;; A rest parameter or nil.
   (%rest-parameter :initform nil
		    :initarg :rest-parameter
		    :reader rest-parameter)
   ;; A list of key parameters.
   (%key-parameters :initform '()
		    :initarg :key-parameters
		    :reader key-parameters)
   ;; Either nil or t
   (%allow-other-keys-parameter :initform nil
				:initarg :allow-other-keys-parameter
				:reader allow-other-keys-parameter)
   ;; A list of aux parameters. 
   (%aux-parameters :initform '()
		    :initarg :aux-parameters
		    :reader aux-parameters)))

(define-condition ordinary-lambda-list-must-be-a-proper-list
    (compilation-program-error)
  ())

(define-condition lambda-list-keyword-illegal-in-ordinary-lambda-list
    (compilation-program-error)
  ())

(define-condition misplaced-lambda-list-keyword
    (compilation-program-error)
  ())

(defun parse-ordinary-lambda-list (lambda-list-ast)
  (unless (or (null (form lambda-list-ast))
	      (and (typep lambda-list-ast 'compound-ast)
		   (proper-list-p (children lambda-list-ast))))
    (error 'ordinary-lambda-list-must-be-a-proper-list
	   :ast lambda-list-ast))
  (flet ((check-for-illegal-lambda-list-keyword (ast)
	   (when (not (member (form ast)
			      '(&optional &rest &key &allow-other-keys &aux)))
	     (error 'lambda-list-keyword-illegal-in-ordinary-lambda-list
		    :ast ast))))
    (if (null (form lambda-list-ast))
	(make-instance 'ordinary-lambda-list)
	(multiple-value-bind (required remaining)
	    (parse-required (children lambda-list-ast) lambda-list-ast)
	  (unless (null remaining)
	    (check-for-illegal-lambda-list-keyword (car remaining)))
	  (multiple-value-bind (optional remaining)
	      (if (and (not (null remaining)) (eq (form (car remaining))
						  '&optional))
		  (parse-optional (cdr remaining) lambda-list-ast)
		  (values '() remaining))
	    (unless (null remaining)
	      (check-for-illegal-lambda-list-keyword (car remaining))
	      (when (member (form (car remaining))
			    '(&optional &allow-other-keys))
		(error 'misplaced-lambda-list-keyword
		       :ast (car remaining))))
	    (multiple-value-bind (rest remaining)
		(if (and (not (null remaining)) (eq (form (car remaining))
						    '&rest))
		    (parse-rest (cdr remaining) lambda-list-ast)
		    (values nil remaining))
	      (unless (null remaining)
		(check-for-illegal-lambda-list-keyword (car remaining))
		(when (member (form (car remaining))
			      '(&optional &rest &allow-other-keys))
		  (error 'misplaced-lambda-list-keyword
			 :ast (car remaining))))
	      (let ((key-present (and (not (null remaining))
				      (eq (form (car remaining)) '&key))))
		(multiple-value-bind (key remaining)
		    (if (and (not (null remaining)) (eq (form (car remaining))
							'&key))
			(parse-key (cdr remaining) lambda-list-ast)
			(values nil remaining))
		  (unless (null remaining)
		    (check-for-illegal-lambda-list-keyword (car remaining))
		    (when (or (member (form (car remaining))
				      '(&optional &rest &key))
			      (and (eq (form (car remaining)) '&allow-other-keys)
				   (not key-present)))
		      (error 'misplaced-lambda-list-keyword
			     :ast (car remaining))))
		  (multiple-value-bind (allow-other-keys remaining)
		      (if (and (not (null remaining)) (eq (form (car remaining))
							  '&allow-other-keys))
			  (values t (cdr remaining))
			  (values nil remaining))
		    (unless (null remaining)
		      (check-for-illegal-lambda-list-keyword (car remaining))
		      (when (member (form (car remaining))
				    '(&optional &rest &key &allow-other-keys))
			(error 'misplaced-lambda-list-keyword
			       :ast (car remaining))))
		    (multiple-value-bind (aux remaining)
			(if (and (not (null remaining)) (eq (form (car remaining))
							    '&aux))
			    (parse-aux (cdr remaining) lambda-list-ast)
			    (values nil remaining))
		      (unless (null remaining)
			(check-for-illegal-lambda-list-keyword (car remaining))
			(when (member (form (car remaining))
				      '(&optional &rest &key &allow-other-keys &aux))
			  (error 'misplaced-lambda-list-keyword
				 :ast (car remaining))))
		      (make-instance 'ordinary-lambda-list
				     :required-parameters required
				     :optional-parameters optional
				     :rest-parameter rest
				     :key-parameters  key
				     :allow-other-keys-parameter allow-other-keys
				     :aux-parameters aux)))))))))))
