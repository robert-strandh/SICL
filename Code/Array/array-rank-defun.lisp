(cl:in-package #:sicl-array)

(defun array-rank (array)
  (length (array-dimensions array)))
