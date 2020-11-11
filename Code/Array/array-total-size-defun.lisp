(cl:in-package #:sicl-array)

(defun array-total-size (array)
  (apply #'* (array-dimensions array)))
