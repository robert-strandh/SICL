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
  (let ((generic-function (sicl-genv:fdefinition function-name environment)))
    (when (null generic-function)
      (let ((keys (copy-list keys)))
	(loop while (remf keys :environment))
	(setf generic-function
	      (apply #'make-instance
		     'standard-generic-function
		     keys))
	(setf (sicl-genv:fdefinition function-name environment)
	      generic-function)))
    generic-function))
	  
