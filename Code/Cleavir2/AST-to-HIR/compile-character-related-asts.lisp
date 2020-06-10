(cl:in-package #:cleavir-ast-to-hir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a CHARACTERP-AST.

(defmethod compile-ast (client (ast cleavir-ast:characterp-ast) context)
  (assert-context ast context 0 2)
  (let ((temp (make-temp)))
    (compile-ast
     client
     (cleavir-ast:object-ast ast)
     (clone-context
      context
      :result temp
      :successor
      (make-instance 'cleavir-ir:characterp-instruction
        :input temp
        :successors (successors context))))))
