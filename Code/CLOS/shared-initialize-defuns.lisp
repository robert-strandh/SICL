(cl:in-package #:sicl-clos)

(defun shared-initialize (instance slot-names &rest initargs)
  (apply #'shared-initialize-default instance slot-names initargs))
