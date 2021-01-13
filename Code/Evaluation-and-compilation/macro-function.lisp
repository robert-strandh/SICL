(cl:in-package #:sicl-evaluation-and-compilation)

(let ((global-environment (sicl-environment:global-environment)))
  (defun macro-function (symbol &optional (environment global-environment))
    (trucler:macro-function symbol environment)))

(let* ((global-environment sicl-environment:global-environment)
       (client (sicl-environment:client global-environment))
       (setf-macro-function-function
         (sicl-environment:fdefinition
          client global-environment '(setf macro-function))))
  (defun (setf macro-function)
      (new-function symbol &optional environment)
    (assert (null environment))
    (funcall setf-macro-function-function
             new-function client global-environment)))
