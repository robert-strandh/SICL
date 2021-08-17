(cl:in-package #:sicl-arithmetic)

(defun lcm (&rest arguments)
  (reduce #'binary-lcm arguments :initial-value 1))
