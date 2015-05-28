(cl:in-package #:sicl-boot)

(defun define-setf-fdefinition (boot)
  (setf (sicl-genv:fdefinition 'fdefinition (r2 boot))
	(lambda (new-definition name)
	  (setf (sicl-genv:fdefinition name (r3 boot))
		new-definition))))
