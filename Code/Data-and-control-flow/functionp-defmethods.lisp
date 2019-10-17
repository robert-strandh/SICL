(cl:in-package #:sicl-data-and-control-flow)

(defmethod functionp (object)
  nil)

(defmethod functionp ((object function))
  t)
