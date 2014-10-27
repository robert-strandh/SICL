(cl:in-package #:sicl-standard-environment-functions)

(defvar *global-environment*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function CONSTANTP.

(defun constantp (form &optional (environment *global-environment*))
  (or (and (not (symbolp form))
	   (not (consp form)))
      (keywordp form)
      (and (symbolp form)
	   (nth-value 1 (sicl-env:constant-variable form environment)))
      (and (consp form)
	   (eq (car form) 'quote))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function BOUNDP.
;;;
;;; According to the HyperSpec, this function should return any true
;;; value if the name is bound in the global environment and false
;;; otherwise.  We return T when the symbol is bound.  
;;;
;;; The HyperSpec does not say whether the name of a constant variable
;;; is considered to be bound.  We think it is reasonable to consider
;;; it bound in this case.  They HyperSpec also does not say whether
;;; the name of a global symbol macro is considered to be bound.
;;; Again, we think it is reasonable to consider this to be the case,
;;; if for nothing else, then for symmetry with fboundp.
;;;
;;; The symbol is bound as a special variable if it is both the case
;;; that a special variable entry exists for it AND the storage cell
;;; of that entry does not contain +unbound+.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function MAKUNBOUND.
;;;
;;; Since we consider a name bound if it names a constant variable, or
;;; if it names a global symbol macro, we must decide what to do in
;;; those cases.  It would be embarassing for someone to call
;;; MAKUNBOUND successfully and then have BOUNDP return true.  What we
;;; do is to remove the symbol macro if any, and signal an error if an
;;; attempt is made to make a constant variable unbound.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function SYMBOL-VALUE.
;;;
;;; The HyperSpec specifically allows for SYMBOL-VALUE to be used on
;;; constant variables.  

(defun symbol-value (symbol)
  (declare (cl:type symbol symbol))
  ;; Handle keyword symbols specially here.
  (if (keywordp symbol)
      symbol
      ;; Next check whether the symbol has a defintion as a constant
      ;; variable. 
      (let ((constant-variable-entry
	      (find symbol (constant-variables *global-environment*)
		    :key #'name :test #'eq)))
	(if (not (null constant-variable-entry))
	    (definition constant-variable-entry)
	    ;; Check whether the symbol has a definition as a special
	    ;; variable, and check whether it is bound. 
	    (let ((special-variable-entry
		    (find symbol (special-variables *global-environment*)
			  :key #'name :test #'eq)))
	      (if (not (null special-variable-entry))
		  (let ((val (car (storage (location special-variable-entry)))))
		    (if (eq val +unbound+)
			(error 'unbound-variable :name symbol)
			val))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function (SETF SYMBOL-VALUE).
;;;
;;; Signal an error if an attempt is made to call this function with
;;; the name of a constant variable.
;;;
;;; The Hyperspec does not indicate what should be done if this
;;; function is called and the name already has a definition as a
;;; global symbol macro.  However, it does say that an error is
;;; signaled in the opposite situation, i.e., if an attempt is made to
;;; define a symbol macro with a name of an existing special variable.
;;; For that reason, we think it is reasonable to signal an error in
;;; this case too.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function (SETF MACRO-FUNCTION).
;;;
;;; The HyperSpec says that the consequences are undefined if a
;;; non-nil environment is given.  We define those consequences to be
;;; that the environment is considered to be a first-class global
;;; environment.

(defun (setf macro-function) (new-function symbol &optional environment)
  (declare (type symbol symbol)
	   (type function new-function))
  (when (null environment)
    (setf environment *global-environment*))
  (setf (sicl-env:macro-function symbol environment)
	new-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function FIND-CLASS.

(defun find-class (symbol &optional (errorp t) environment)
  (when (null environment)
    (setf environment *global-environment*))
  (let ((class (sicl-env:find-class symbol environment)))
    (if (and (null class) errorp)
	(error "There is no class named ~s" symbol)
	class)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function (SETF FIND-CLASS).

(defun (setf find-class) (new-class symbol &optional errorp environment)
  (declare (ignore errorp))
  (when (null environment)
    (setf environment *global-environment*))
  (setf (sicl-env:find-class symbol environment)
	new-class))
