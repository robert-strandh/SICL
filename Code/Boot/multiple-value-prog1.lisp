(cl:in-package #:sicl-boot)

(defmethod cleavir-ast-to-hir:compile-ast
    ((ast cleavir-ast:multiple-value-prog1-ast) context)
  (cleavir-ast-to-hir:compile-ast
   (cleavir-ast:first-form-ast ast)
   (cleavir-ast-to-hir:context
    (cleavir-ast-to-hir::results context)
    (list (let ((successor (car (cleavir-ast-to-hir::successors context))))
	    (loop for ast in (reverse (cleavir-ast:form-asts ast))
		  do (setf successor
			   (cleavir-ast-to-hir:compile-ast
			    ast
			    (cleavir-ast-to-hir:context
			     '()
			     (list successor)
			     (cleavir-ast-to-hir::invocation context)))))
	    successor))
    (cleavir-ast-to-hir::invocation context))))
