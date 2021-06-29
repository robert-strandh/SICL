(cl:in-package #:sicl-clos)

;;; Return true if and only if OBJECT is a class.
(defgeneric classp (object))

(defmethod classp (object)
  nil)

(defmethod classp ((object class))
  t)
