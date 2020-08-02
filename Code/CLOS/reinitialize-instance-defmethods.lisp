(cl:in-package #:sicl-clos)

(defmethod reinitialize-instance
    ((instance standard-object) &rest initargs &key &allow-other-keys)
  (apply #'reinitialize-instance-default instance initargs))

(defmethod reinitialize-instance :around
    ((class real-class) &rest initargs &key &allow-other-keys)
  (apply #'reinitialize-instance-around-real-class-default
         #'call-next-method
         class
         initargs))
