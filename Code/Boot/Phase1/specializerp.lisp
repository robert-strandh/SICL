(cl:in-package #:sicl-boot-phase1)

(defgeneric specializerp (object))

(defmethod specializerp (object)
  (declare (ignore object))
  nil)

(defmethod specializerp ((object specializer))
  (declare (ignorable object))
  t)
