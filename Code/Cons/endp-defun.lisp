(cl:in-package #:sicl-cons)

(defun endp (list)
  (unless (typep list 'list)
    (error 'must-be-list
           :datum list
           :name 'endp))
  (null list))
