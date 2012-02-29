(in-package :sicl-code-utilities)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; We will attempt to represent global environments as objects,
;;; rather than the traditional method of spreading out the global
;;; environment inside value cells, function cells, etc. 
;;;
;;; By doing it this way, we gain several advantages.  For one thing,
;;; we can easily have different threads use different global
;;; environments.  For another, we can easily separate the STARTUP
;;; ENVIRONMENT (the global environment from which a compilation was
;;; started) from the EVALUATION ENVIRONMENT (the environment used by
;;; the compiler to evaluate forms that must be evaluated at compile
;;; time).  The evaluation environment "inherits" from the startup
;;; environment (whatever that means), which we will take to mean that
;;; the startup environment is cloned at the beginning of each
;;; compilation in order to create the evaluation environment.  This
;;; separation means that no evaluations made during the compilation
;;; will persist in THE runtime environment after the compilation
;;; finished.
;;;
;;; Letting different threads use different global runtime
;;; environments makes it easier to create a multi-user system.
;;;
;;; We have to be careful that this representation of environments
;;; does not make performance-critical code unacceptably slow.  Some
;;; things will inevitably be a bit slower, for instance SYMBOL-VALUE
;;; or FDEFINITION when invoked with non-constant names.  But we must
;;; make sure that invoking a function by its global name as in the
;;; form (F ARG1 ARG2 ...) does not require going through the
;;; environment object at each function call, while still allowing F
;;; to be redefined at runtime.  We solve this problem by having the
;;; constants vector of a compilation unit refer not to the symbol F
;;; but to a CONS cell containing the definition of the function F.
;;; That CONS cell is part of the environment, and shared by all
;;; compilation units that use this environment.  Translating from the
;;; symbol F to the CONS cell is done a load time. 

(defclass environment () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The global environment.

(defclass global-environment (environment)
  (;; Functions whose names are symbols.
   (%symbol-functions
    :initform (make-hash-table :test #'eq)
    :reader symbol-functions)
   ;; Functions whose names are of the form
   ;; (setf SYMBOL) and SYMBOL is the key.
   (%setf-functions
    :initform (make-hash-table :test #'eq)
    :reader setf-functions)
   (%macros
    :initform (make-hash-table :test #'eq)
    :reader macros)
   (%special-operators
    :initform (make-hash-table :test #'eq)
    :reader special-operators)
   ;; Compiler macros for functions whose names are symbols.
   (%compiler-macros
    :initform (make-hash-table :test #'eq)
    :reader compiler-macros)
   ;; Compiler macros for functions whose names are of the form
   ;; (setf SYMBOL) and SYMBOL is the key.
   (%setf-compiler-macros
    :initform (make-hash-table :test #'eq)
    :reader setf-compiler-macros)
   (%special-variables
    :initform (make-hash-table :test #'eq)
    :reader special-variables)
   (%constant-variables
    :initform (make-hash-table :test #'eq)
    :reader constant-variables)
   (%type-names
    :initform (make-hash-table :test #'eq)
    :reader type-names)
   (%class-names
    :initform (make-hash-table :test #'eq)
    :reader class-names)
   (%proclamations
    :initform '()
    :reader proclamations)))

;;; FIXME: This should really be a clone of some other environment.
(defparameter *compiler-environment*
  (make-instance 'global-environment))

;; ;;; Enter a special for testing purposes.
;; (setf (gethash '*print-base* (specials *compiler-environment*)) t)

;;; Some standard operators that use a global environment

;;; Do this better when we have control over internal representation.
;;; Then, use an immediate value instead. 
(defvar *secret-unbound-value* (list nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Check if something is bound.

(defun boundp-in-table (symbol table)
  (let ((cell (gethash symbol table)))
    (and (not (null cell))
	 (not (eq (car cell) *secret-unbound-value*)))))

(defun boundp-in-environment (symbol environment)
  (declare (type symbol symbol)
	   (type global-environment environment))
  (boundp-in-table symbol (special-variables environment)))

(defun fboundp-in-environment (name environment)
  (declare (type function-name name)
	   (type global-environment environment))
  (if (symbolp name)
      (or (boundp-in-table name (symbol-functions environment))
	  (boundp-in-table name (macros environment))
	  (boundp-in-table name (special-operators environment)))
      (boundp-in-table (cadr name) (setf-functions environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Make something unbound.

(defun makunbound-in-table (symbol table)
  (let ((cell (gethash symbol table)))
    (or (null cell)
	(progn 
	  (setf cell (list *secret-unbound-value*))
	  (setf (gethash symbol table) cell)))))

(defun makunbound-in-environment (symbol environment)
  (declare (type symbol symbol)
	   (type global-environment environment))
  (makunbound-in-table symbol (special-variables environment)))

(defun fmakunbound-in-environment (name environment)
  (declare (type function-name name)
	   (type global-environment environment))
  (if (symbolp name)
      (progn (makunbound-in-table name (symbol-functions environment))
	     (makunbound-in-table name (macros environment)))
      (makunbound-in-table (cadr name) (setf-functions environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Get the value of the binding of something.

;;; Warning: this function doesn't signal the cell-error condition
;;; when the cell is unbound.
(defun value-in-table (symbol table)
  (let ((cell (gethash symbol table)))
    (if (null cell)
	*secret-unbound-value*
	(car cell))))

(defun symbol-function-in-environment (symbol environment)
  (declare (type symbol symbol)
	   (type global-environment environment))
  (flet ((return-if-bound-in-table (symbol table)
	   (let ((value (value-in-table symbol table)))
	     (unless (eq value *secret-unbound-value*)
	       (return-from symbol-function-in-environment value)))))
    (return-if-bound-in-table symbol (symbol-functions environment))
    (return-if-bound-in-table symbol (macros environment))
    (return-if-bound-in-table symbol (special-operators environment)))
  (error 'sicl-undefined-function
	 :name symbol
	 :signaler 'symbol-function))

(defun fdefinition-in-environment (name environment)
  (declare (type function-name name)
	   (type global-environment environment))
  (flet ((return-if-bound-in-table (symbol table)
	   (let ((value (value-in-table symbol table)))
	     (unless (eq value *secret-unbound-value*)
	       (return-from fdefinition-in-environment value)))))
    (cond ((symbolp name)
	   (return-if-bound-in-table name (symbol-functions environment))
	   (return-if-bound-in-table name (macros environment))
	   (return-if-bound-in-table name (special-operators environment)))
	  (t
	   (return-if-bound-in-table (cadr name) (symbol-functions environment)))))
  (error 'sicl-undefined-function
	 :name name
	 :signaler 'fdefinition))

(defun symbol-value-in-environment (symbol environment)
  (declare (type symbol symbol)
	   (type global-environment environment))
  (flet ((return-if-bound-in-table (symbol table)
	   (let ((value (value-in-table symbol table))) 
	     (unless (eq value *secret-unbound-value*)
	       (return-from symbol-value-in-environment value)))))
    (return-if-bound-in-table symbol (special-variables environment)))
  (error 'sicl-unbound-variable
	 :name symbol
	 :signaler 'symbol-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Set the value of the binding of something.

(defun set-value-in-table (value symbol table)
  (let ((cell (gethash symbol table)))
    (if (null cell)
	(setf (gethash symbol table)
	      (list value))
	(setf (car cell) value))))

(defun (setf symbol-function-in-environment) (new-value symbol environment)
  (declare (type symbol symbol)
	   (type function new-value))
  (set-value-in-table new-value symbol (symbol-functions environment)))

(defun (setf fdefinition-in-environment) (new-value name environment)
  (declare (type function new-value)
	   (type function-name name))
  (if (symbolp name)
      (setf (symbol-function-in-environment name environment) new-value)
      (set-value-in-table new-value (cadr name) (setf-functions environment))))

(defun (setf symbol-value-in-environment) (new-value symbol environment)
  (declare (type symbol symbol))
  (set-value-in-table new-value symbol (special-variables environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lexical environments

(defclass delta-environment (environment)
  ((%parent :initarg :parent :reader parent)))

;;; A binding environment is an environment that reflects the
;;; creation of some kind of binding.
(defclass binding-environment (delta-environment)
  ((%name :initarg :name :reader name)))

;;; An environment that contains a declaration. 
(defclass declaration-environment (delta-environment)
  ((%item :initarg :item :reader item)))

;;; A referencing declaration is one of dynamic-extent, ftype,
;;; ignorable, ignore, inline, notinline, special, or type.  It can
;;; also be an implementation-defined declaration referring to some
;;; name.  The target is the binding to which the declaration
;;; applies.
(defclass referencing-declaration-environment (declaration-environment)
  ((%target :initarg :target :reader target)))

(defclass pure-declaration-envirionment (declaration-environment)())

;;; This mixin is used for environments that will require runtime
;;; allocation.
(defclass allocating-environment-mixin () ())

;;; A variable environment is a binding environment that has to do with
;;; the namespace of variables. 
(defclass variable-environment (binding-environment)
  ())

(defclass lexical-variable-environment
    (variable-environment allocating-environment-mixin)
  ())

(defclass special-variable-environment (variable-environment)
  ())

(defclass symbol-macro-environment (variable-environment)
  ((%definition :initarg :definition :reader definition)))

;;; A function environment is a binding environment that has to do with
;;; the namespace of functions. 
(defclass function-environment (binding-environment)
  ())

(defclass lexical-function-environment
    (function-environment allocating-environment-mixin)
  ())

(defclass macro-environment (function-environment)
  ((%expander :initarg expander :reader expander)))

(defclass block-environment (binding-environment allocating-environment-mixin)
  ())

(defclass tag-environment (binding-environment allocating-environment-mixin)
  ())

(defun find-global-environment (environment)
  (loop for env = environment then (parent env)
	when (typep env 'global-environment)
	  return env))

(defun variable-information (name &optional env)
  (labels ((find-binding (env)
	     (cond ((typep env 'global-environment)
		    (or (gethash name (special-variables env))
			(gethash name (constant-variables env))))
		   ((and (typep env 'variable-environment)
			 (eq name (name env)))
		    env)
		   (t
		    (find-binding (parent env)))))
	   (find-declarations (env binding)
	     (cond ((typep env 'global-environment)
		    (cdr (or (gethash name (special-variables env))
			     (gethash name (constant-variables env)))))
		   ((eq env binding)
		    '())
		   ((and (typep env 'referencing-declaration-environment)
			 (eq (target env) binding))
		    (cons (item env)
			  (find-declarations (parent env) binding)))
		   (t
		    (find-declarations (parent env) binding)))))
    (let ((binding (find-binding env))
	  (global (find-global-environment env)))
      (if (null binding)
	  (values nil nil '())
	  (let ((declarations (find-declarations env binding)))
	    (cond ((consp binding)
		   (if (eq binding
			   (gethash name (special-variables global)))
		       (values :special nil declarations)
		       (values :constant nil '())))
		  ((typep binding 'symbol-macro-environment)
		   (values :symbol-macro binding '()))
		  ((typep binding 'special-variable-environment)
		   (values :special binding declarations))
		  ((typep binding 'lexical-variable-environment)
		   (values :lexical binding declarations))
		  (t
		   (assert nil))))))))

(defun function-information (name &optional env)
  (labels ((find-binding (env)
	     (cond ((typep env 'global-environment)
		    (or (and (consp name)
			     (gethash (cadr name) (setf-functions env)))
			(gethash name (symbol-functions env))
			(gethash name (macros env))
			(gethash name (special-operators env))))
		   ((and (typep env 'function-environment)
			 (equal name (name env)))
		    env)
		   (t
		    (find-binding (parent env)))))
	   (find-declarations (env binding)
	     (cond ((typep env 'global-environment)
		    (cdr (or (and (consp name)
				  (gethash (cadr name) (setf-functions env)))
			     (gethash name (symbol-functions env))
			     (gethash name (macros env))
			     (gethash name (special-operators env)))))
		   ((eq env binding)
		    '())
		   ((and (typep env 'referencing-declaration-environment)
			 (eq (target env) binding))
		    (cons (item env)
			  (find-declarations (parent env) binding)))
		   (t
		    (find-declarations (parent env) binding)))))
    (let ((binding (find-binding env))
	  (global (find-global-environment env)))
      (if (null binding)
	  (values nil nil '())
	  (let ((declarations (find-declarations env binding)))
	    (cond ((consp binding)
		   (cond ((or (and (consp binding)
				   (eq binding
				       (gethash (cadr name) (setf-functions global))))
			      (gethash name (symbol-functions global)))
			  (values :function nil declarations))
			 ((eq binding
			      (gethash name (macros global)))
			  (values :macro nil declarations))
			 (t
			  (values :special-operator nil declarations))))
		  ((typep binding 'macro-environment)
		   (values :macro binding declarations))
		  ((typep binding 'lexical-function-environment)
		   (values :function binding declarations))
		  (t
		   (assert nil))))))))

(defun block-information (name &optional env)
  (labels ((find-binding (env)
	     (cond ((typep env 'global-environment)
		    nil)
		   ((and (typep env 'block-environment)
			 (equal name (name env)))
		    env)
		   (t
		    (find-binding (parent env))))))
    (let ((binding (find-binding env)))
      (if (null binding)
	  (values nil nil '())
	  (values :block binding '())))))

(defun tag-information (name &optional env)
  (labels ((find-binding (env)
	     (cond ((typep env 'global-environment)
		    nil)
		   ((and (typep env 'tag-environment)
			 (equal name (name env)))
		    env)
		   (t
		    (find-binding (parent env))))))
    (let ((binding (find-binding env)))
      (if (null binding)
	  (values nil nil '())
	  (values :tag binding '())))))

;;; FIXME: do more error checking here
(defun augment-environment (env
			    &key
			      ((:variable variables))
			      ((:symbol-macro symbol-macros))
			      ((:function functions))
			      ((:macro macros))
			      ((:block blocks))
			      ((:tag tags))
			      ((:declare declares)))
  (let ((result env))
    (loop for variable in variables
	  do (setf env (make-instance 'lexical-variable-environment
				      :parent env
				      :name variable)))
    (loop for function in functions
	  do (setf env (make-instance 'lexical-function-environment
				      :parent env
				      :name function)))
    (loop for symbol-macro in symbol-macros
	  do (setf env (make-instance 'symbol-macro-environment
				      :parent env
				      :name (car symbol-macro)
				      :definition (cadr symbol-macro))))
    (loop for macro in macros
	  do (setf env (make-instance 'macro-environment
				      :parent env
				      :name (car macro)
				      :definition (cadr macro))))
    (loop for block in blocks
	  do (setf env (make-instance 'block-environment
				      :parent env
				      :name block)))
    (loop for tag in tags
	  do (setf env (make-instance 'lexical-function-environment
				      :parent env
				      :name tag)))
;;; FIXME: distinguish between pure and referencing declarations
    (loop for item in declares
	  do (setf env (make-instance 'declaration-environment
				      :parent env
				      :item item)))
    result))
