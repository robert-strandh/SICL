(cl:in-package #:sicl-boot-phase1)

(defgeneric classp (object))

(defmethod classp (object)
  (declare (ignore object))
  nil)

(defmethod classp ((object class))
  (declare (ignorable object))
  t)
