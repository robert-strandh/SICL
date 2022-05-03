(cl:in-package #:sicl-evaluation-and-compilation)

(let* ((global-environment (env:global-environment))
       (client (env:client global-environment))
       (compiler-macro-function-function
         (fdefinition 'env:compiler-macro-function)))
  (defun compiler-macro-function (symbol)
    (funcall compiler-macro-function-function client global-environment symbol)))
