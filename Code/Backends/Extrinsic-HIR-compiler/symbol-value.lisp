(cl:in-package #:sicl-extrinsic-hir-compiler)

(defun find-variable-entry (symbol dynamic-env)
  (loop for entry in dynamic-env
	do (when (and (typep entry 'variable-binding)
		      (eq (symbol entry) symbol))
	     (return entry))))

(defun symbol-value (symbol dynamic-env unbound-value global-value-cell)
  (let ((entry (find-variable-entry symbol dynamic-env)))
    (if (null entry)
	(if (eq (car global-value-cell) unbound-value)
	    (error "unbound variable ~s" symbol)
	    (car global-value-cell))
	(if (eq (value entry) unbound-value)
	    (error "unbound variable ~s" symbol)
	    (value entry)))))

(defun (setf symbol-value) (new-value symbol dynamic-env global-value-cell)
  (let ((entry (find-variable-entry symbol dynamic-env)))
    (if (null entry)
	(setf (car global-value-cell) new-value)
	(setf (value entry) new-value))))
