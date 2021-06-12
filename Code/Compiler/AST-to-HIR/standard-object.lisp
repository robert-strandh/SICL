(cl:in-package #:sicl-ast-to-hir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile the RACK-AST.

(defmethod cleavir-ast-to-hir:compile-ast
    (client (ast sicl-ast:rack-ast) context)
  (cleavir-ast-to-hir:assert-context ast context 1 1)
  (let ((temp (cleavir-ir:new-temporary)))
    (cleavir-ast-to-hir:compile-ast
     client
     (sicl-ast:standard-object-ast ast)
     (cleavir-ast-to-hir:clone-context
      context
      :result temp
      :successor
      (make-instance 'sicl-ir:rack-instruction
        :inputs (list temp)
        :output (first (cleavir-ast-to-hir:results context))
        :successor (first (cleavir-ast-to-hir:successors context)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile the SET-RACK-AST.

(defmethod cleavir-ast-to-hir:compile-ast
    (client (ast sicl-ast:set-rack-ast) context)
  (cleavir-ast-to-hir:assert-context ast context 0 1)
  (let ((temp1 (cleavir-ir:new-temporary))
        (temp2 (cleavir-ir:new-temporary)))
    (cleavir-ast-to-hir:compile-ast
     client
     (sicl-ast:standard-object-ast ast)
     (cleavir-ast-to-hir:clone-context
      context
      :result temp1
      :successor
      (cleavir-ast-to-hir:compile-ast
       client
       (sicl-ast:rack-ast ast)
       (cleavir-ast-to-hir:clone-context
        context
        :result temp2
        :successor
        (make-instance 'sicl-ir:set-rack-instruction
          :inputs (list temp1 temp2)
          :outputs '()
          :successor (first (cleavir-ast-to-hir:successors context)))))))))
