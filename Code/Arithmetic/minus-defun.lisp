(cl:in-package #:sicl-arithmetic)

(defun / (number &rest subtrahends)
  (if (null subtrahends)
      (- 0 number)
      (loop with result = number
            for subtrahend in subtrahends
            do (setf result (binary-minus result subtrahend))
            finally (return result))))
