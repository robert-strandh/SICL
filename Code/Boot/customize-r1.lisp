(cl:in-package #:sicl-boot)

(defun define-ensure-generic-function-r1 (boot)
  (setf (sicl-genv:fdefinition 'ensure-generic-function (r1 boot))
	(lambda (function-name &rest arguments)
	  (let ((args (copy-list arguments)))
	    (loop while (remf args :environment))
	    (if (sicl-genv:fboundp function-name (r1 boot))
		(sicl-genv:fdefinition function-name (r1 boot))
		(setf (sicl-genv:fdefinition function-name (r1 boot))
		      (apply #'make-instance 'standard-generic-function
			     args)))))))

(defun customize-r1 (boot)
  (let ((c (c1 boot))
	(r (r1 boot)))
    (message "Customizing run-time environment R1~%")
    (define-ensure-generic-function-r1 boot)
    (message "Finished customizing run-time environment R1~%")))
