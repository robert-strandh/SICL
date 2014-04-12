(cl:in-package #:cleavir-ast-transformations)

(defgeneric fixup (clone original dictionary))

(defmethod fixup (clone original dictionary)
  (reinitialize-instance
   clone
   :children (loop for child in (cleavir-ast:children original)
		   collect (gethash child dictionary))))

(defgeneric clone-node (ast))

(defmethod clone-node (ast)
  (make-instance (class-of ast)))

(defmethod clone-node :around ((ast cleavir-ast:immediate-ast))
  (reinitialize-instance
   (call-next-method)
   :value (cleavir-ast:value ast)))

(defmethod clone-node :around ((ast cleavir-ast:constant-ast))
  (reinitialize-instance
   (call-next-method)
   :value (cleavir-ast:value ast)))

(defmethod clone-node :around ((ast cleavir-ast:global-ast))
  (reinitialize-instance
   (call-next-method)
   :name (cleavir-ast:name ast)
   :function-type (cleavir-ast:function-type ast)))

(defmethod clone-node :around ((ast cleavir-ast:special-ast))
  (reinitialize-instance
   (call-next-method)
   :name (cleavir-ast:name ast)))

(defmethod clone-node :around ((ast cleavir-ast:lexical-ast))
  (reinitialize-instance
   (call-next-method)
   :name (cleavir-ast:name ast)))
