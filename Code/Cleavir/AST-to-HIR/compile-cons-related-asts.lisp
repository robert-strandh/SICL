(cl:in-package #:cleavir-ast-to-hir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a CONSP-AST.

(defmethod compile-ast (client (ast cleavir-ast:consp-ast) context)
  (assert-context ast context 0 2)
  (let ((temp (make-temp)))
    (compile-ast
     client
     (cleavir-ast:object-ast ast)
     (clone-context
      context
      :result temp
      :successor
      (make-instance 'cleavir-ir:consp-instruction
        :input temp
        :successors (successors context))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a CAR-AST

(define-compile-functional-ast
    cleavir-ast:car-ast cleavir-ir:car-instruction
  (cleavir-ast:cons-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a CDR-AST

(define-compile-functional-ast
    cleavir-ast:cdr-ast cleavir-ir:cdr-instruction
  (cleavir-ast:cons-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a RPLACA-AST

(define-compile-functional-ast
    cleavir-ast:rplaca-ast cleavir-ir:rplaca-instruction
  (cleavir-ast:cons-ast cleavir-ast:object-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a RPLACD-AST

(define-compile-functional-ast
    cleavir-ast:rplacd-ast cleavir-ir:rplacd-instruction
  (cleavir-ast:cons-ast cleavir-ast:object-ast))
