(cl:in-package #:sicl-arithmetic)

(defgeneric floatp (object)
  (:method (object)
    (declare (ignore object))
    nil)
  (:method ((object float))
    t))
