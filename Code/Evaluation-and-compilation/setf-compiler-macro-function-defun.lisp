(cl:in-package #:sicl-evaluation-and-compilation)

(let* ((global-environment (sicl-environment:global-environment))
       (client (sicl-environment:client global-environment))
       (setf-compiler-macro-function-function
         (fdefinition '(setf sicl-environment:compiler-macro-function))))
  (defun (setf compiler-macro-function) (new-function symbol)
    (funcall setf-compiler-macro-function-function new-function client global-environment symbol)))
