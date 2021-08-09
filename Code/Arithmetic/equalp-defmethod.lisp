(cl:in-package #:sicl-arithmetic)

(defmethod equalp ((x number) (y number))
  (= x y))
