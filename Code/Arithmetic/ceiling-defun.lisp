(cl:in-package #:sicl-arithmetic)

(defun ceiling (number &optional (divisor 1))
  (generic-ceiling number divisor))
