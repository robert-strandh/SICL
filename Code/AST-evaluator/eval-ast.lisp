(cl:in-package #:sicl-ast-evaluator)

(defun eval-ast (ast environment)
  (let* ((client (sicl-environment:client environment))
         (code (translate-top-level-ast client ast)))
    (funcall (compile nil code) environment)))
