(cl:in-package #:sicl-cons)

(defun endp (list)
  (unless (listp list)
    (error 'must-be-list
           :datum list))
  (null list))
