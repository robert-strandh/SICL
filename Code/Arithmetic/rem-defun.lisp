(cl:in-package #:sicl-arithmetic)

(defun rem (number divisor)
  (nth-value 1 (truncate number divisor)))
