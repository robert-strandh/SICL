(cl:in-package #:cleavir-ast-transformations)

(defgeneric fixup (clone original dictionary))

(defmethod fixup (clone original dictionary)
  (reinitialize-instance
   clone
   :children (loop for child in (cleavir-ast:children original)
		   collect (gethash child dictionary))))

(defun clone-ast (ast)
  (let ((dictionary (make-hash-table :test #'eq)))
    (labels ((traverse (node)
	       (when (null (gethash node dictionary))
		 (setf (gethash node dictionary)
		       (make-instance (class-of node)))
		 (mapc #'traverse (cleavir-ast:children node)))))
      (traverse ast))
    (maphash (lambda (original clone)
	       (fixup clone original dictionary))
	     dictionary)))
			
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
