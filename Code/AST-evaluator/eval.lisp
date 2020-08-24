(cl:in-package #:sicl-ast-evaluator)

(defun eval (form environment)
  (funcall (compile nil (translate-code form environment))
           environment))
