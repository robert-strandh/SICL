(cl:in-package #:sicl-arithmetic)

(defun + (&rest arguments)
  (reduce #'binary-plus arguments :initial-value 0))
