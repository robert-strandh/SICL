(cl:in-package #:sicl-data-and-control-flow)

(defgeneric equalp (x y))

(defmethod equalp (x y)
  (declare (ignore x y))
  nil)

(defmethod equalp :around (x y)
  (or (eq x y)
      (call-next-method)))
