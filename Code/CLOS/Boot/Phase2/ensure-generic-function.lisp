(cl:in-package #:sicl-clos)

(defun ensure-generic-function (function-name &rest args)
  (if (bridge-generic-function-exists-p function-name)
      (find-bridge-generic-function function-name)
      (let ((fun (apply #'cl:make-instance
			'bridge-generic-function
			:name function-name
			args)))
	(add-bridge-generic-function function-name fun)
	fun)))
