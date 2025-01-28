(cl:in-package #:sicl-arithmetic)

(defmethod binary-logxor ((x fixnum) (y fixnum))
  (po:primop :fixnum-logxor x y))
