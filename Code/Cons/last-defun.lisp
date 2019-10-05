(cl:in-package #:sicl-cons)

(defun last (list &optional (n 1))
  (unless (typep list 'list)
    (error 'must-be-list
           :datum list
           :name 'last))
  (unless (typep n '(integer 0))
    (error 'must-be-nonnegative-integer
           :datum n
           :name 'last))
  (let ((remaining list))
    (loop repeat n
          until (atom remaining)
          do (setf remaining (cdr remaining)))
    (loop until (atom remaining)
          do (setf list (cdr list))
          do (setf remaining (cdr remaining)))
    list))
