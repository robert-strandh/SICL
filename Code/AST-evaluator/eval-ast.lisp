(cl:in-package #:sicl-ast-evaluator)

(defun eval-ast (client environment ast)
  (let ((code (translate-top-level-ast client ast)))
    (funcall (compile nil code) environment)))
