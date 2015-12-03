(cl:in-package #:sicl-boot)

(defun define-class-prototype (r)
  (setf (sicl-genv:fdefinition 'sicl-clos:class-prototype r)
	#'closer-mop:class-prototype))
