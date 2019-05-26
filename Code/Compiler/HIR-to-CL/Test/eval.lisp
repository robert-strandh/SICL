(cl:in-package #:sicl-hir-to-cl-test)

(defun eval (client form environment)
  (let ((cst (cst:cst-from-expression form)))
    (sicl-hir-to-cl:cst-eval client cst environment)))
