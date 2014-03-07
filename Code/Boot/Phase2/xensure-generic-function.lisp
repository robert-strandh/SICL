(cl:in-package #:sicl-boot-phase2)

(defun *ensure-generic-function (function-name &rest args)
  (let ((fun (find-target-generic-function function-name nil)))
    (if (null fun)
	(let ((new-fun (apply #'make-instance
			      'standard-generic-function
			      :name function-name
			      args)))
	  (add-target-generic-function function-name new-fun)
	  new-fun)
	fun)))
