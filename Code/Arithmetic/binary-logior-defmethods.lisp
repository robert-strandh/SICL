(cl:in-package #:sicl-arithmetic)

(defmethod binary-logior ((x fixnum) (y fixnum))
  (po:primop :fixnum-logior x y))
