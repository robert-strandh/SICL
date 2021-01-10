(cl:in-package #:sicl-evaluation-and-compilation)

(let* ((global-environment (sicl-environment:global-environment environment))
       (client (sicl-environment:client global-environment))
       (compiler-macro-function-function
         (fdefinition 'sicl-environment:compiler-macro-function)))
  (defun compiler-macro-function (symbol)
    (funcall compiler-macro-function-function client global-environment symbol)))
