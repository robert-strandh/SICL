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

(defmethod cleavir-ast-to-hir:compile-ast
    (client (ast sicl-ast:establish-stack-frame-ast) context)
  (cleavir-ast-to-hir:assert-context ast context 0 1)
  (let ((temp1 (cleavir-ir:new-temporary))
        (temp2 (cleavir-ir:new-temporary)))
    (cleavir-ast-to-hir:compile-ast
     client
     (sicl-ast:stack-pointer-ast ast)
     (cleavir-ast-to-hir:clone-context
      context
      :result temp1
      :successor
      (cleavir-ast-to-hir:compile-ast
       client
       (sicl-ast:frame-pointer-ast ast)
       (cleavir-ast-to-hir:clone-context
        context
        :result temp2
        :successor
        (make-instance 'sicl-ir:establish-stack-frame-instruction
          :inputs (list temp1 temp2)
          :successor (first (cleavir-ast-to-hir:successors context)))))))))
