(cl:in-package #:sicl-cons)

(defun nreconc (list tail)
  (loop with remaining = list
        with result = tail
        until (atom remaining)
        do (let ((temp (cdr remaining)))
             (setf (cdr remaining) result
                   result remaining
                   remaining temp))
        finally (unless (null remaining)
                  (error 'must-be-proper-list
                         :datum list
                         :name 'nreconc))
                (return result)))
