(cl:in-package #:sicl-minimal-extrinsic-environment)

;;;; This file must not be loaded into the extrinsic environment.  The
;;;; functions defined here must be compiled and executed in the host
;;;; environment.  The reason for that is that they access the dynamic
;;;; environment, which is the value of the host special variable
;;;; *DYNAMIC-ENVIRONMENT* when a function is called.

(defun find-variable-entry (symbol dynamic-environment)
  (loop for entry in dynamic-environment
	do (when (and (typep entry 'variable-binding)
		      (eq (symbol entry) symbol))
	     (return entry))))

(defun symbol-value (symbol env)
  (let* ((dynamic-environment *dynamic-environment*)
	 (entry (find-variable-entry symbol dynamic-environment))
	 (unbound-value (sicl-global-environment:variable-unbound symbol env))
	 (global-value-cell (sicl-global-environment:variable-cell symbol env)))
    (if (null entry)
	(if (eq (car global-value-cell) unbound-value)
	    (error "unbound variable ~s" symbol)
	    (car global-value-cell))
	(if (eq (value entry) unbound-value)
	    (error "unbound variable ~s" symbol)
	    (value entry)))))

(defun (setf symbol-value) (new-value symbol env)
  (let* ((dynamic-environment *dynamic-environment*)
	 (entry (find-variable-entry symbol dynamic-environment))
	 (global-value-cell (sicl-global-environment:variable-cell symbol env)))
    (if (null entry)
	(setf (car global-value-cell) new-value)
	(setf (value entry) new-value))))
