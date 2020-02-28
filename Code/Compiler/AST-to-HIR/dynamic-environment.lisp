(cl:in-package #:sicl-ast-to-hir)

(defmethod cleavir-ast-to-hir:compile-ast
    (client (ast sicl-ast:dynamic-environment-ast) context)
  (cleavir-ast-to-hir:assert-context ast context 1 1)
  (make-instance 'sicl-ir:dynamic-environment-instruction
    :inputs '()
    :output (first (cleavir-ast-to-hir:results context))
    :successor (first (cleavir-ast-to-hir:successors context))))
