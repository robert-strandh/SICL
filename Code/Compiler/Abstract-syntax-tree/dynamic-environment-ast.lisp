(cl:in-package #:sicl-ast)

(defclass dynamic-environment-ast
    (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ())

(cleavir-io:define-save-info dynamic-environment-ast)

(defmethod cleavir-ast:children ((ast dynamic-environment-ast))
  '())
