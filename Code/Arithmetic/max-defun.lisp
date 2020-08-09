(cl:in-package #:sicl-arithmetic)

(defun max (x &rest args)
  (loop with max = x
        for arg in args
        when (> arg max)
          do (setf max arg)
        finally (return max)))
