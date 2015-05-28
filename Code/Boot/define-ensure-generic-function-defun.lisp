(cl:in-package #:sicl-boot)

(defun define-ensure-generic-function (c r)
  (setf (sicl-genv:fdefinition 'ensure-generic-function r)
	(let ((ensure-generic-function  (sicl-genv:fdefinition
					 'ensure-generic-function
					 c)))
	  (lambda (function-name &rest arguments)
	    (if (sicl-genv:fboundp function-name r)
		(sicl-genv:fdefinition function-name r)
		(let ((new-arguments (copy-list arguments)))
		  (loop while (remf new-arguments :environment))
		  (setf (sicl-genv:fdefinition function-name r)
			(apply ensure-generic-function
			       (gensym)
			       new-arguments))))))))
