(cl:in-package #:sicl-arithmetic)

(defgeneric numberp (object)
  (:method (object)
    (declare (ignore object))
    nil)
  (:method ((object number))
    t))
