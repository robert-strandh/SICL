(cl:in-package #:sicl-extrinsic-hir-compiler)

(defun symbol-value (symbol dynamic-env unbound-value global-value-cell)
  (loop for entry in dynamic-env
	do (when (and (typep entry 'variable-binding)
		      (eq (symbol entry) symbol))
	     (if (eq (value entry) unbound-value)
		 (funcall error "unbound variable ~s" symbol)
		 (return (value entry))))
	finally
	   (if (eq (car global-value-cell) unbound)
	       (funcall error "unbound variable ~s" symbol)
	       (return (car global-value-cell)))))
