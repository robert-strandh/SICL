(cl:in-package #:sicl-boot)

(defun define-direct-slot-definition-class (boot)
  (setf (sicl-genv:fdefinition 'sicl-clos:direct-slot-definition-class (r2 boot))
	(lambda (&rest arguments)
	  (declare (ignore arguments))
	  (sicl-genv:find-class 'sicl-clos:standard-direct-slot-definition
				(r1 boot)))))
