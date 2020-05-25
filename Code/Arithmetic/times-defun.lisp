(cl:in-package #:sicl-arithmetic)

(defun * (&rest arguments)
  (reduce #'binary-times arguments :initial-value 1))
