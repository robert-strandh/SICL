(cl:in-package #:sicl-form-to-ast)

(defun form-to-ast (client form environment)
  (let ((cst (cst:cst-from-expression form)))
    (cleavir-cst-to-ast:cst-to-ast client cst environment)))
