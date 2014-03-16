(cl:in-package #:sicl-boot-phase3)

(defun *ensure-generic-function (function-name &rest args)
  (let ((fun (find-ersatz-generic-function function-name nil)))
    (if (null fun)
	(let ((new-fun (apply #'make-instance
			      'standard-generic-function
			      :name function-name
			      args)))
	  (add-ersatz-generic-function function-name new-fun)
	  new-fun)
	fun)))
