(cl:in-package #:sicl-hir-interpreter-test)

(defun eval (client form environment)
  (let ((cst (cst:cst-from-expression form)))
    (sicl-hir-interpreter:cst-eval client cst environment)))
