(cl:in-package #:sicl-arithmetic)

(defun round (number &optional (divisor 1))
  (generic-round number divisor))
