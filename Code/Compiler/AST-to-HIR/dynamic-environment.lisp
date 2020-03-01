(cl:in-package #:sicl-ast-to-hir)

(defmethod cleavir-ast-to-hir:compile-ast
    (client (ast sicl-ast:dynamic-environment-ast) context)
  (cleavir-ast-to-hir:assert-context ast context 1 1)
  (make-instance 'sicl-ir:dynamic-environment-instruction
    :inputs '()
    :output (first (cleavir-ast-to-hir:results context))
    :successor (first (cleavir-ast-to-hir:successors context))))

(defmethod cleavir-ast-to-hir:compile-ast
    (client (ast sicl-ast:with-dynamic-environment-ast) context)
  (cleavir-ast-to-hir:assert-context ast context nil 1)
  (let ((new-dynamic-environment-location
          (cleavir-ir:make-lexical-location "denv")))
    (cleavir-ast-to-hir:compile-ast
     client
     (sicl-ast:dynamic-environment-ast ast)
     (cleavir-ast-to-hir:clone-context
      context
      :result new-dynamic-environment-location
      :successor
      (cleavir-ast-to-hir:compile-ast
       client
       (sicl-ast:body-ast ast)
       (cleavir-ast-to-hir:clone-context
        context
        :dynamic-environment-location new-dynamic-environment-location))))))
