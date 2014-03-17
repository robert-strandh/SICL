(cl:in-package #:sicl-boot-phase2)

(defparameter *find-ersatz-generic-function*
  #'find-ersatz-generic-function)

(defparameter *add-ersatz-generic-function*
  #'add-ersatz-generic-function)

(defun *ensure-generic-function (function-name &rest args)
  (let ((fun (funcall *find-ersatz-generic-function* function-name nil)))
    (if (null fun)
	(let ((new-fun (apply #'make-instance
			      'standard-generic-function
			      :name function-name
			      args)))
	  (funcall *add-ersatz-generic-function* function-name new-fun)
	  new-fun)
	fun)))
