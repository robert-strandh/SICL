(cl:in-package #:sicl-clos)

(defmethod reinitialize-instance
    ((instance standard-object) &rest initargs &key &allow-other-keys)
  (apply #'reinitialize-instance-default instance initargs))
