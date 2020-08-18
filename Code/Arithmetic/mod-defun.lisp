(cl:in-package #:sicl-arithmetic)

(defun mod (number divisor)
  (nth-value 1 (floor number divisor)))
