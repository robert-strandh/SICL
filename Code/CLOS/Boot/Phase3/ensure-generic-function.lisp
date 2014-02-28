(cl:in-package #:sicl-clos)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (fmakunbound 'ensure-generic-function))

(defun ensure-generic-function (function-name &rest args)
  (let ((fun (find-target-generic-function function-name nil)))
    (if (null fun)
	(let* ((class (find-bridge-class 'standard-generic-function))
	       (method-class (find-bridge-class 'standard-method))
	       (new-fun (apply #'make-instance class
			       :name function-name
			       :method-class method-class
			       args)))
	  (add-target-generic-function function-name new-fun)
	  new-fun)
	fun)))
