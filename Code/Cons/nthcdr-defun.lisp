(cl:in-package #:sicl-cons)

(defun nthcdr (n list)
  (unless (typep n '(integer 0))
    (error 'must-be-nonnegative-integer
           :datum n
           :name 'nthcdr))
  (loop until (zerop n)
        until (atom list)
        do (decf n)
        do (setf list (cdr list)))
  (when (and (plusp n) (not (null list)))
    (error 'must-be-cons
           :datum list
           :name 'nthcdr))
  list)
