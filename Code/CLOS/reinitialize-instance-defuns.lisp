(cl:in-package #:sicl-clos)

(defun reinitialize-instance (instance &rest initargs &key &allow-other-keys)
  (apply #'reinitialize-instance-default instance initargs))
