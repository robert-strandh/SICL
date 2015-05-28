(cl:in-package #:sicl-boot)

(defun define-generic-function-method-class (r)
  (setf (sicl-genv:fdefinition 'sicl-clos:generic-function-method-class
			       r)
	#'closer-mop:generic-function-method-class))

