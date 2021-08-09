(cl:in-package #:sicl-arithmetic)

(defmethod equal ((x number) (y number))
  (eql x y))
