(cl:in-package #:sicl-package)

(defgeneric packagep (object)
  (:method (object)
    (declare (ignore object))
    nil)
  (:method ((object package))
    (declare (ignorable object))
    t))
