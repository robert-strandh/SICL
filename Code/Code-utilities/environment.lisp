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
  ((%parent :initarg :parent :reader parent)
   (%identifier :initarg :identifier :reader identifier)
   (%declarations :initarg :declarations :reader declarations)))

(defclass variable-environment (delta-environment) ())
(defclass function-environment (delta-environment) ())

(defclass macro-environment (function-environment)
  ((%expander :initarg expander :reader expander)))

(defclass binding-environment-mixin () ())

(defun find-binding (identifier environment type)
  (labels ((aux (environment level)
	     (cond ((typep environment 'global-environment)
		    nil)
		   ((and (typep environment type)
			 (eq identifier (identifier environment)))
		    level)
		   ((typep environment 'binding-environment-mixin)
		    (aux (parent environment) (1+ level)))
		   (t (aux (parent environment) level)))))
    (aux environment 0)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variable environment

(defclass lexical-variable-environment
    (variable-environment binding-environment-mixin)
  ())

(defgeneric variable-information-using-class (identifier env))

(defmethod variable-information-using-class (identifier env)
  (variable-information-using-class identifier (parent env)))

(defmethod variable-information-using-class (identifier (env variable-environment))
  (values :lexical env '()))

;;; FIXME: Clearly wrong.
(defmethod variable-information-using-class (identifier (env global-environment))
  (let ((constant (gethash identifier (constant-variables env))))
    (if (not (null constant))
	(values :constant nil '((type . t)))
	(let ((special (gethash identifier (special-variables env))))
	  (if (not (null special))
	      (values :special nil '((type . t)))
	      (values nil nil nil))))))

(defun variable-information (identifier &optional env)
  (variable-information-using-class identifier (or env *compiler-environment*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function environment

(defclass lexical-function-environment
    (function-environment binding-environment-mixin)
  ())

(defgeneric function-information-using-class (identifier env))

(defmethod function-information-using-class (identifier env)
  (function-information-using-class identifier (parent env)))

(defmethod function-information-using-class (identifier (env function-environment))
  (values :function env '()))

(defmethod function-information-using-class (identifier (env macro-environment))
  (values :macro env '()))

;;; FIXME: Clearly wrong.
(defmethod function-information-using-class (identifier (env global-environment))
  (let ((macro (gethash identifier (macros env))))
    (if (not (null macro))
	(values :macro nil '((type . t)))
	(let ((function (gethash identifier (symbol-functions env))))
	  (if (not (null function))
	      (values :function nil '((type . t)))
	      (let ((special-form (gethash identifier (special-operators env))))
		(if (not (null special-form))
		    (values :special-form nil '((type . t)))
		    (values nil nil nil))))))))
  
(defun function-information (identifier &optional env)
  (function-information-using-class identifier (or env *compiler-environment*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Block environment

(defclass block-environment (delta-environment binding-environment-mixin) ())

(defgeneric block-information-using-class (identifier env))

(defmethod block-information-using-class (identifier env)
  (block-information-using-class identifier (parent env)))

(defmethod block-information-using-class (identifier (env block-environment))
  (values :block env '()))

(defmethod block-information-using-class (identifier (env global-environment))
  (values nil nil ()))

(defun block-information (identifier &optional env)
  (block-information-using-class identifier (or env *compiler-environment*)))

