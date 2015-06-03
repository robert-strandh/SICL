(cl:in-package #:sicl-clos)

(defmethod shared-initialize :around
    ((class real-class)
     slot-names
     &rest initargs
     &key
     &allow-other-keys)
  (apply #'shared-initialize-around-real-class-default
	 #'call-next-method
	 class
	 slot-names
	 initargs))
