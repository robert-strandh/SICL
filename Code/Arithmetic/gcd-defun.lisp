(cl:in-package #:sicl-arithmetic)

(defun gcd (&rest arguments)
  (reduce #'binary-gcd arguments :initial-value 0))
