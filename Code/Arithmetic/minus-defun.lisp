(cl:in-package #:sicl-arithmetic)

(defun - (number &rest subtrahends)
  (if (null subtrahends)
      (- 0 number)
      (reduce #'binary-subtract subtrahends :initial-value number)))
