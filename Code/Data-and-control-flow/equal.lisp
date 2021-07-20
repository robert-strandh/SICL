(cl:in-package #:sicl-data-and-control-flow)

(defgeneric equal (x y))

(defmethod equal (x y)
  (declare (ignore x y))
  nil)

(defmethod equal :around (x y)
  (or (eq x y)
      (call-next-method)))

