(cl:in-package #:sicl-form-to-ast)

(defun form-to-ast (client form environment &key file-compilation-semantics)
  (let ((cst (cst:cst-from-expression form)))
    (cleavir-cst-to-ast:cst-to-ast
     client cst environment
     :file-compilation-semantics file-compilation-semantics)))
