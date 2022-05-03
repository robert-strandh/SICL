(cl:in-package #:sicl-clos)

(defun (setf find-class) (new-class symbol &optional errorp environment)
  (declare (ignore errorp))
  (let* ((global-environment (env:global-environment environment))
         (client (env:client global-environment)))
    (setf (env:find-class client global-environment symbol)
          new-class)))
