(cl:in-package #:sicl-clos)

(defmethod allocate-instance ((class standard-class) &rest initargs)
  (apply #'allocate-instance-default class initargs))

(defmethod allocate-instance ((class funcallable-standard-class) &rest initargs)
  (apply #'allocate-instance-default class initargs))
