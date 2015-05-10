(cl:in-package #:sicl-clos)

(defun ensure-generic-function (function-name
				&rest keys
				&key
				argument-precedence-order
				declare
				documentation
				environment
				generic-function-class
				lambda-list
				method-class
				method-combination)
  (declare (ignore argument-precedence-order
		   declare
		   documentation
		   generic-function-class
		   lambda-list
		   method-class
		   method-combination))
  (if (sicl-genv:fboundp function-name environment)
      ;; We do not check whether it is something other than a generic
      ;; function, such as an ordinary function or a macro.
      (sicl-genv:fdefinition function-name environment)
      (let ((keys (copy-list keys)))
	(loop while (remf keys :environment))
	(let ((generic-function (apply #'make-instance
				       'standard-generic-function
				       :name function-name
				       keys)))
	  (setf (sicl-genv:fdefinition function-name environment)
		generic-function)
	  generic-function))))
