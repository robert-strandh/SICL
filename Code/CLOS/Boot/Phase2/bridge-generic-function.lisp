(cl:in-package #:sicl-clos)

(cl:defclass bridge-generic-function (cl:standard-generic-function
					 standard-generic-function)
  ()
  (:metaclass #.(cl:class-name
		 (cl:class-of
		  (cl:find-class 'cl:standard-generic-function)))))

(defmethod cl:no-applicable-method
    ((generic-function bridge-generic-function) &rest args)
  (apply (discriminating-function generic-function) args))
