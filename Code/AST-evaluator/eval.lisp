(cl:in-package #:sicl-ast-evaluator)

(defun eval (cst environment)
  (funcall (compile nil (translate-code cst environment))
           environment))
