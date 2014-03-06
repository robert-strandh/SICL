(cl:in-package #:sicl-clos)

(defmethod make-built-in-instance ((class symbol) &rest initargs)
  (apply #'make-built-in-instance (find-class class) initargs))

(defmethod make-built-in-instance ((class built-in-class) &rest initargs)
  (apply #'make-built-in-instance-default
	 class
	 #'initialize-built-in-instance
	 initargs))
