(cl:in-package #:sicl-arithmetic)

(defun /= (argument &rest arguments)
  (if (null arguments)
      t
      (and (loop for y in arguments
                 never (binary-equal argument y))
           (apply #'/= arguments))))
