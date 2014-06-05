(cl:in-package #:cleavir-ast-to-mir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a AREF-AST

(defmethod compile-ast ((ast cleavir-ast:aref-ast) context)
  (check-context-for-one-value-ast context)
  (let ((temp1 (make-temp nil))
	(temp2 (make-temp nil)))
    (compile-ast
     (cleavir-ast:array-ast ast)
     (context
      (list temp1)
      (list (compile-ast
	     (cleavir-ast:index-ast ast)
	     (context (list temp2)
		      (list (make-instance 'cleavir-mir:aref-instruction
			      :inputs (list temp1 temp2)
			      :outputs (results context)
			      :successors (successors context))))))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a ASET-AST

(defmethod compile-ast ((ast cleavir-ast:aset-ast) context)
  (check-context-for-no-value-ast context)
  (let ((temp1 (make-temp nil))
	(temp2 (make-temp nil))
	(temp3 (make-temp nil)))
    (compile-ast
     (cleavir-ast:array-ast ast)
     (context
      (list temp1)
      (list (compile-ast
	     (cleavir-ast:index-ast ast)
	     (context
	      (list temp2)
	      (compile-ast
	       (cleavir-ast:value-ast ast)
	       (context
		(list temp3)
		(list (make-instance 'cleavir-mir:aset-instruction
			:inputs (list temp1 temp2 temp3)
			:outputs '()
			:successors (successors context))))))))))))
