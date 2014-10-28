(cl:in-package #:sicl-extrinsic-environment)

(defclass environment (sicl-simple-environment:simple-environment)
  ())

(defmethod cleavir-env:eval (form environment1 (environment2 environment))
  (let ((ast (cleavir-generate-ast:generate-ast form environment1)))
    (cleavir-ast-interpreter:interpret ast)))
