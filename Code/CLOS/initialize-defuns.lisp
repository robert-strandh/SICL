(cl:in-package #:sicl-clos)

(defun initialize-instance (instance &rest initargs &key &allow-other-keys)
  (apply #'initialize-instance-default instance initargs))

(defun reinitialize-instance (instance &rest initargs &key &allow-other-keys)
  (apply #'reinitialize-instance-default instance initargs))

(defun shared-initialize (instance slot-names &rest initargs)
  (apply #'shared-initialize-default instance slot-names initargs))
