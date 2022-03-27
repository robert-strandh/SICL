(cl:in-package #:sicl-cst-to-ast)

(defun cst-to-ast
    (client cst compilation-environment file-compilation-semantics-p)
  (cleavir-cst-to-ast:cst-to-ast
   client cst compilation-environment
   :file-compilation-semantics file-compilation-semantics-p))
