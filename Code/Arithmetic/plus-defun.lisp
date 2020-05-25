(cl:in-package #:sicl-arithmetic)

(defun + (&rest arguments)
  (reduce #'binary-add arguments :initial-value 0))
