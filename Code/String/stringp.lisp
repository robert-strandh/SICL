(cl:in-package #:sicl-string)

(defgeneric stringp (object))

(defmethod stringp (object)
  (declare (ignore object))
  nil)

(defmethod stringp ((object string))
  (declare (ignorable object))
  t)
