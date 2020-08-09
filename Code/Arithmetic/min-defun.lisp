(cl:in-package #:sicl-arithmetic)

(defun min (x &rest args)
  (loop with min = x
        for arg in args
        when (< arg min)
          do (setf min arg)
        finally (return min)))
