(cl:in-package #:sicl-arithmetic)

(defun * (&rest arguments)
  (reduce #'binary-multiply arguments :initial-value 1))
