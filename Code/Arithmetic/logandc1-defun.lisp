(cl:in-package #:sicl-arithmetic)

(defun logandc1 (integer-1 integer-2)
  (logand (lognot integer-1) integer-2))
