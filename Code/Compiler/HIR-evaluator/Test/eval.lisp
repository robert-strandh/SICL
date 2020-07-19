(cl:in-package #:sicl-hir-evaluator-test)

(defun eval (client form environment)
  (let ((cst (cst:cst-from-expression form)))
    (sicl-hir-evaluator:cst-eval client cst environment)))
