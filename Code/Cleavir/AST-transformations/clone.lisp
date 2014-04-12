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
			
(defmethod fixup :after ((clone cleavir-ast:immediate-ast) original dictionary)
  (declare (ignore dictionary))
  (reinitialize-instance
   clone
   :value (cleavir-ast:value original)))

(defmethod fixup :after ((clone cleavir-ast:constant-ast) original dictionary)
  (declare (ignore dictionary))
  (reinitialize-instance
   clone
   :value (cleavir-ast:value original)))

(defmethod fixup :after ((clone cleavir-ast:global-ast) original dictionary)
  (declare (ignore dictionary))
  (reinitialize-instance
   clone
   :name (cleavir-ast:name original)
   :function-type (cleavir-ast:function-type original)))

(defmethod fixup :after ((clone cleavir-ast:special-ast) original dictionary)
  (declare (ignore dictionary))
  (reinitialize-instance
   clone
   :name (cleavir-ast:name original)))

(defmethod fixup :after ((clone cleavir-ast:lexical-ast) original dictionary)
  (declare (ignore dictionary))
  (reinitialize-instance
   clone
   :name (cleavir-ast:name original)))
