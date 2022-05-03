(cl:in-package #:sicl-evaluation-and-compilation)

(let* ((global-environment (env:global-environment))
       (client (env:client global-environment))
       (setf-compiler-macro-function-function
         (fdefinition '(setf env:compiler-macro-function))))
  (defun (setf compiler-macro-function) (new-function symbol)
    (funcall setf-compiler-macro-function-function new-function client global-environment symbol)))
