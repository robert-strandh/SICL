(cl:in-package #:sicl-clos)

(cl:defmethod cl:print-object ((object heap-instance) stream)
  (print-unreadable-object (object stream)
    (format stream "an instance of ~s"
	    (funcall (find-bridge-generic-function 'class-name)
		     (heap-instance-class object)))))
