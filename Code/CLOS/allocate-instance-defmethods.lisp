(cl:in-package #:sicl-clos)

(defmethod allocate-instance ((class regular-class) &rest initargs)
  (apply #'allocate-instance-default class initargs))
