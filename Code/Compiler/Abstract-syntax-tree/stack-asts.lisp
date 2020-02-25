(cl:in-package #:sicl-ast)

(defclass caller-stack-pointer-ast
    (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ())

(defclass caller-frame-pointer-ast
    (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ())
