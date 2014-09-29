(cl:in-package #:cleavir-ast-interpreter)

(defgeneric interpret-ast (ast static-env dynamic-env))
