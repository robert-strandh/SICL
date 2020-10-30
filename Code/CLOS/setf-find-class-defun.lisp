(cl:in-package #:sicl-clos)

(defun (setf find-class) (new-class symbol &optional errorp environment)
  (declare (ignore errorp))
  (let* ((global-environment (sicl-environment:global-environment environment))
         (client (sicl-environment:client global-environment)))
    (setf (sicl-environment:find-class client global-environment symbol)
          new-class)))
