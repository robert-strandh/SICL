(cl:in-package #:sicl-stream)

(defclass stream () ())

(defmethod streamp (object)
  (declare (ignore object))
  nil)

(defmethod streamp ((object stream))
  (declare (ignorable object))
  t)
