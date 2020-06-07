(cl:in-package #:sicl-cons)

(defun nth (n list)
  ;; (unless (typep n '(integer 0))
  ;;   (error 'must-be-nonnegative-integer
  ;;          :datum n
  ;;          :name 'nthcdr))
  (loop until (zerop n)
        until (atom list)
        do (decf n)
        do (setf list (cdr list)))
  ;; (when (not (listp list))
  ;;   (error 'must-be-list
  ;;          :datum list
  ;;          :name 'nth))
  (car list))
