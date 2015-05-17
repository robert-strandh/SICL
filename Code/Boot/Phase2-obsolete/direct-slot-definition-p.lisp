(cl:in-package #:sicl-boot-phase2)

(defgeneric direct-slot-definition-p (object))

(defmethod direct-slot-definition-p (object)
  (declare (ignore object))
  nil)

(defmethod direct-slot-definition-p ((object direct-slot-definition))
  (declare (ignorable object))
  t)
