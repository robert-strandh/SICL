(cl:in-package #:sicl-clos)

(defun initialize-instance (instance &rest initargs &key &allow-other-keys)
  (apply #'initialize-instance-default instance initargs))
