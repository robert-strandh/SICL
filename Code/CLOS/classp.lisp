(cl:in-package #:sicl-clos)

(defmethod classp (object)
  nil)

(defmethod classp ((object class))
  t)
