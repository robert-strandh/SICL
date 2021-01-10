(cl:in-package #:sicl-evaluation-and-compilation)

(let* ((global-environment (sicl-environment:global-environment environment))
       (client (sicl-environment:client global-environment))
       (special-operator-p-function
         (fdefinition 'sicl-environment:special-operator-p)))
  (defun special-operator-p (symbol)
    (funcall special-operator-p-function client global-environment symbol)))
