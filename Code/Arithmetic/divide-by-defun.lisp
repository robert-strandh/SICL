(cl:in-package #:sicl-arithmetic)

(defun / (number &rest denominators)
  (if (null denominators)
      (/ 1 number)
      (reduce #'binary-divide denominators :initial-value number)))
