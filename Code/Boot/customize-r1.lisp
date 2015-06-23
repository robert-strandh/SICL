(cl:in-package #:sicl-boot)

;;; We define a special version of ENSURE-GENERIC-FUNCTION in the
;;; run-time environment R1.  While the function is defined in the
;;; run-time environment R1, it operates on the run-time environment
;;; R2.  This version checks whether there is already a function named
;;; FUNCTION-NAME in R2.  If so that function is returned, and it is
;;; assumed to be a generic function.  If not, an instance of the host
;;; class STANDARD-GENERIC-FUNCTION is created and associated with
;;; FUNCTION-NAME in R2.
(defun define-ensure-generic-function-r1 (boot)
  (setf (sicl-genv:fdefinition 'ensure-generic-function (r1 boot))
	(lambda (function-name &rest arguments)
	  (let ((args (copy-list arguments)))
	    (loop while (remf args :environment))
	    (if (sicl-genv:fboundp function-name (r2 boot))
		(sicl-genv:fdefinition function-name (r2 boot))
		(setf (sicl-genv:fdefinition function-name (r2 boot))
		      (apply #'make-instance 'standard-generic-function
			     :name function-name
			     args)))))))

(defun customize-r1 (boot)
  (let ((c (c1 boot))
	(r (r1 boot)))
    (message "Customizing run-time environment R1~%")
    (define-ensure-generic-function-r1 boot)
    (message "Finished customizing run-time environment R1~%")))
