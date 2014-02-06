(cl:in-package #:sicl-clos)

(defmethod allocate-instance ((class standard-class) &rest initargs)
  (declare (ignore initargs))
  (allocate-instance-default class))

(defmethod allocate-instance ((class funcallable-standard-class) &rest initargs)
  (declare (ignore initargs))
  (allocate-instance-default class))
