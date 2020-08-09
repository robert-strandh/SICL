(cl:in-package #:sicl-arithmetic)

(defun < (argument &rest arguments)
  (loop for x = argument then y
        for y in arguments
        always (binary-less x y)))
