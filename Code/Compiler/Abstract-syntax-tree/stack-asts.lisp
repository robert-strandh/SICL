(cl:in-package #:sicl-ast)

(defclass caller-stack-pointer-ast
    (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ())

(cleavir-io:define-save-info caller-stack-pointer-ast)

(defmethod cleavir-ast:children ((ast caller-stack-pointer-ast))
  '())

(defclass caller-frame-pointer-ast
    (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ())

(cleavir-io:define-save-info caller-frame-pointer-ast)

(defmethod cleavir-ast:children ((ast caller-frame-pointer-ast))
  '())
