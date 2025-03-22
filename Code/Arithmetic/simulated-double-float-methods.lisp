(cl:in-package #:sicl-arithmetic)

(defmethod binary-equal
    ((x simulated-double-float) (y simulated-double-float))
  (binary-equal (bit-pattern x) (bit-pattern y)))

(defmethod integer-decode-float ((float simulated-double-float))
  (buoy-simulate:integer-decode-double-float (bit-pattern float)))
