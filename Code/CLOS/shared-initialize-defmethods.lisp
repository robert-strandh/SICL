(cl:in-package #:sicl-clos)

(defmethod shared-initialize
    ((instance standard-object) slot-names &rest initargs)
  (apply #'shared-initialize-default-using-class
         instance
         slot-names
         (class-of instance)
         initargs))

(defmethod shared-initialize
    ((instance function) slot-names &rest initargs)
  (apply #'shared-initialize-default-using-class
         instance
         slot-names
         (class-of instance)
         initargs))
