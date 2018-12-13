(cl:in-package #:sicl-printer)

(defun print-ratio (ratio base stream)
  (print-integer (numerator ratio) base stream)
  (print-integer (denominator ratio) base stream))
