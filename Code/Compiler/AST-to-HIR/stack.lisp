(cl:in-package #:sicl-ast-to-hir)

(defmethod cleavir-ast-to-hir:compile-ast
    (client (ast sicl-ast:caller-stack-pointer-ast) context)
  (cleavir-ast-to-hir:assert-context ast context 1 1)
  (make-instance 'sicl-ir:caller-stack-pointer-instruction
    :inputs '()
    :output (first (cleavir-ast-to-hir:results context))
    :successor (first (cleavir-ast-to-hir:successors context))))

(defmethod cleavir-ast-to-hir:compile-ast
    (client (ast sicl-ast:caller-frame-pointer-ast) context)
  (cleavir-ast-to-hir:assert-context ast context 1 1)
  (make-instance 'sicl-ir:caller-frame-pointer-instruction
    :inputs '()
    :output (first (cleavir-ast-to-hir:results context))
    :successor (first (cleavir-ast-to-hir:successors context))))
