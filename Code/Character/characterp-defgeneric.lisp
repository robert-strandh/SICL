(cl:in-package #:sicl-character)

(defgeneric characterp (object)
  (:method (object)
    (declare (ignore object))
    nil)
  (:method ((object character))
    t))
