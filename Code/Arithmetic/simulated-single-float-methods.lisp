(cl:in-package #:sicl-arithmetic)

(defmethod binary-equal
    ((x simulated-single-float) (y simulated-single-float))
  (binary-equal (bit-pattern x) (bit-pattern y)))
