(cl:in-package #:sicl-cons)

(defun (setf nth) (object n list)
  (unless (typep n '(integer 0))
    (error 'must-be-nonnegative-integer
           :datum n
           :name '(setf nth)))
  (loop with remaining = list
        with count = 0
        until (atom remaining)
        until (= count n)
        do (setf remaining (cdr remaining))
           (incf count)
        finally  (when (not (consp remaining))
                   (error 'setf-nth-must-be-cons
                          :datum remaining
                          :name 'nth
                          :original-tree list
                          :cons-cell-count count))
                 (setf (car remaining) object))
  object)
