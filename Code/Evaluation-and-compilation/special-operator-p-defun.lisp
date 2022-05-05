(cl:in-package #:sicl-evaluation-and-compilation)

(let* ((global-environment (env:global-environment environment))
       (special-operator-p-function
         (fdefinition 'env:special-operator-p)))
  (defun special-operator-p (symbol)
    (funcall special-operator-p-function
             sicl-client:*client* client global-environment symbol)))
