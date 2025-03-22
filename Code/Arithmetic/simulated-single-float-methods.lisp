(cl:in-package #:sicl-arithmetic)

(defgeneric integer-decode-float (float))

(defmethod binary-equal
    ((x simulated-single-float) (y simulated-single-float))
  (binary-equal (bit-pattern x) (bit-pattern y)))

(defmethod integer-decode-float ((float simulated-single-float))
  (buoy-simulate:integer-decode-single-float (bit-pattern float)))
