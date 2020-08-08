(cl:in-package #:sicl-arithmetic)

(defgeneric realp (object)
  (:method (object)
    (declare (ignore object))
    nil)
  (:method ((object real))
    t))
