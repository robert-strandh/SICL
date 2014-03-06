(cl:in-package #:sicl-clos)

(defmethod initialize-built-in-instance
    (instance &rest initargs &key &allow-other-keys)
  (apply #'initialize-built-in-instance-default instance initargs))
