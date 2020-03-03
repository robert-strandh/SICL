(cl:in-package #:cleavir-ast-to-hir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a SLOT-READ-AST

(defmethod compile-ast ((ast cleavir-ast:slot-read-ast) context)
  (let ((temp1 (make-temp))
	(temp2 (make-temp)))
    (compile-ast
     (cleavir-ast:object-ast ast)
     (clone-context
      context
      :results (list temp1)
      :successors
      (list (compile-ast
	     (cleavir-ast:slot-number-ast ast)
	     (clone-context
              context
              :results (list temp2)
              :successors
              (list (make-instance 'cleavir-ir:slot-read-instruction
                      :inputs (list temp1 temp2)
                      :outputs (results context)
                      :successors (successors context))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a SLOT-WRITE-AST

(defmethod compile-ast ((ast cleavir-ast:slot-write-ast) context)
  (let ((temp1 (make-temp))
	(temp2 (make-temp))
	(temp3 (make-temp)))
    (compile-ast
     (cleavir-ast:object-ast ast)
     (clone-context
      context
      :results (list temp1)
      :successors
      (list (compile-ast
	     (cleavir-ast:slot-number-ast ast)
	     (clone-context
              context
	      :results (list temp2)
              :successors
	      (list (compile-ast
                     (cleavir-ast:value-ast ast)
                     (clone-context
                      context
                      :results (list temp3)
                      :successors
                      (list (make-instance 'cleavir-ir:slot-write-instruction
                              :inputs (list temp1 temp2 temp3)
                              :outputs '()
                              :successors (successors context)))))))))))))
