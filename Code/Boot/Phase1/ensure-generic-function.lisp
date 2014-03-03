(cl:in-package #:sicl-boot-phase1)

(defun ensure-generic-function (function-name &rest args)
  (let ((fun (find-bridge-generic-function function-name nil)))
    (if (null fun)
	(let ((new-fun (apply #'cl:make-instance
			      'bridge-generic-function
			      :name function-name
			      args)))
	  (add-bridge-generic-function function-name new-fun)
	  new-fun)
	fun)))
