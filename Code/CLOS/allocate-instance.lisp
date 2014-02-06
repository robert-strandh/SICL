(cl:in-package #:sicl-clos)

(defmethod allocate-instance ((class standard-class) &rest initargs)
  (declare (ignore initargs))
  (allocate-instance-default class))

(defmethod allocate-instance ((class funcallable-standard-class) &rest initargs)
  (declare (ignore initargs))
  (allocate-instance-default class))

(defun standard-instance-access (instance location)
  (slot-contents (heap-instance-slots instance) location))

(defun (setf standard-instance-access) (new-value instance location)
  (setf (slot-contents (heap-instance-slots instance) location) new-value))

