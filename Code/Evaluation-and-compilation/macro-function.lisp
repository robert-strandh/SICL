(cl:in-package #:sicl-evaluation-and-compilation)

(let ((global-environment (env:global-environment)))
  (defun macro-function (symbol &optional (environment global-environment))
    (trucler:macro-function symbol environment)))

(symbol-macrolet ((client sicl-client:*client*))
  (let* ((global-environment env:global-environment)
         (client (env:client global-environment))
         (setf-macro-function-function
           (env:fdefinition
            client global-environment '(setf macro-function))))
    (defun (setf macro-function)
        (new-function symbol &optional environment)
      (assert (null environment))
      (funcall setf-macro-function-function
               new-function client global-environment))))
