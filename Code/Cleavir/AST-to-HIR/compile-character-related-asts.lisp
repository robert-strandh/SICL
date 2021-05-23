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
      :successors
      (list (make-instance 'cleavir-ir:characterp-instruction
              :input temp
              :successors (successors context)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a CHAR-CODE-AST

(define-compile-functional-ast
    cleavir-ast:char-code-ast cleavir-ir:char-code-instruction
  (cleavir-ast:char-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a CODE-CHAR-AST

(define-compile-functional-ast
    cleavir-ast:code-char-ast cleavir-ir:code-char-instruction
  (cleavir-ast:code-ast))
