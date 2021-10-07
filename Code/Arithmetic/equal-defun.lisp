(cl:in-package #:sicl-arithmetic)

(defun = (argument &rest arguments)
  (loop for y in arguments
        always (binary-equal argument y)))
