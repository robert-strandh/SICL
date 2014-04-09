(cl:in-package #:cleavir-ast-to-mir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-ARITHMETIC-AST.

(defmethod compile-ast ((ast cleavir-ast:fixnum-arithmetic-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (unless (and (= (length results) 1)
		 (= (length successors) 1))
      (error "Invalid context for FIXNUM-ARITHMETIC-AST."))
    (let ((temp (find-or-create-location (cleavir-ast:variable-ast ast)))
	  (normal (compile-ast (cleavir-ast:normal-ast ast) context))
	  (overflow (compile-ast (cleavir-ast:overflow-ast ast) context)))
      (compile-ast (cleavir-ast:operation-ast ast)
		   (context (list temp) (list normal overflow))))))
