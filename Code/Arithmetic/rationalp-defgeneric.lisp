(cl:in-package #:sicl-arithmetic)

(defgeneric rationalp (object)
  (:method (object)
    (declare (ignore object))
    nil)
  (:method ((object rational))
    t))
