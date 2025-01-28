(cl:in-package #:sicl-arithmetic)

(defmethod binary-logand ((x fixnum) (y fixnum))
  (po:primop :fixnum-logand x y))
