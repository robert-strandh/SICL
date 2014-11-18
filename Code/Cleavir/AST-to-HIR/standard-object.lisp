(cl:in-package #:cleavir-ast-to-hir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a SLOT-READ-AST

(defmethod compile-ast ((ast cleavir-ast:slot-read-ast) context)
  (check-context-for-one-value-ast context)
  (let ((temp1 (make-temp))
	(temp2 (make-temp)))
    (compile-ast
     (cleavir-ast:object-ast ast)
     (context
      (list temp1)
      (list (compile-ast
	     (cleavir-ast:slot-number-ast ast)
	     (context (list temp2)
		      (list (make-instance 'cleavir-ir:slot-read-instruction
			      :inputs (list temp1 temp2)
			      :outputs (results context)
			      :successors (successors context)))
		      (invocation context))))
      (invocation context)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a SLOT-WRITE-AST

(defmethod compile-ast ((ast cleavir-ast:slot-write-ast) context)
  (check-context-for-no-value-ast context)
  (let ((temp1 (make-temp))
	(temp2 (make-temp))
	(temp3 (make-temp)))
    (compile-ast
     (cleavir-ast:object-ast ast)
     (context
      (list temp1)
      (list (compile-ast
	     (cleavir-ast:slot-number-ast ast)
	     (context
	      (list temp2)
	      (compile-ast
	       (cleavir-ast:value-ast ast)
	       (context
		(list temp3)
		(list (make-instance 'cleavir-ir:slot-write-instruction
			:inputs (list temp1 temp2 temp3)
			:outputs '()
			:successors (successors context)))
		(invocation context)))
	      (invocation context))))
      (invocation context)))))
