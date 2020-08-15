(cl:in-package #:sicl-arithmetic)

(defun floor (number &optional (divisor 1))
  (generic-floor number divisor))
