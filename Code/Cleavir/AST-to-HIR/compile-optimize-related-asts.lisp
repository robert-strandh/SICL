(cl:in-package #:cleavir-ast-to-hir)

(defmethod compile-ast ((ast cleavir-ast:speed-ast) context)
  (compile-ast (cleavir-ast:child-ast ast)
	       (clone-context context
			      :speed-value (cleavir-ast:value ast))))
