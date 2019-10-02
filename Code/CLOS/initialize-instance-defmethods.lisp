(cl:in-package #:sicl-clos)

(defmethod initialize-instance
    ((instance standard-object) &rest initargs &key &allow-other-keys)
  (apply #'initialize-instance-default instance initargs))
