(cl:in-package #:sicl-clos)

(defmethod allocate-instance ((class regular-class) &rest initargs)
  (apply #'allocate-instance-regular-class class initargs))

(defmethod allocate-instance ((class built-in-class) &rest initargs)
  (apply #'allocate-instance-built-in-class class initargs))
