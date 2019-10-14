(cl:in-package #:sicl-clos)

(defmethod shared-initialize
    ((instance standard-object) slot-names &rest initargs)
  (apply #'shared-initialize-default instance slot-names initargs))
