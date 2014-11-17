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

(defmethod fixup :after ((clone cleavir-ast:function-ast) original dictionary)
  (let ((new-lambda-list
	  (loop for item in (cleavir-ast:lambda-list original)
		collect (cond ((member item lambda-list-keywords)
			       item)
			      ((and (consp item) (= (length item) 3))
			       `(,(first item)
				 ,(gethash (second item) dictionary)
				 ,(gethash (third item) dictionary)))
			      ((and (consp item) (= (length item) 2))
			       `(,(gethash (second item) dictionary)
				 ,(gethash (third item) dictionary)))
			      (t
			       (gethash item dictionary))))))
    (reinitialize-instance
     clone
     :lambda-list new-lambda-list)))

(defmethod fixup :after ((clone cleavir-ast:tag-ast) original dictionary)
  (declare (ignore dictionary))
  (reinitialize-instance
   clone
   :name (cleavir-ast:name original)))

(defmethod fixup :after ((clone cleavir-ast:the-ast) original dictionary)
  (declare (ignore dictionary))
  (reinitialize-instance
   clone
   :value-type (cleavir-ast:value-type original)))

(defmethod fixup :after ((clone cleavir-ast:typeq-ast) original dictionary)
  (declare (ignore dictionary))
  (reinitialize-instance
   clone
   :type-specifier (cleavir-ast:type-specifier original)))

(defmethod fixup :after ((clone cleavir-ast:load-time-value-ast)
			 original dictionary)
  (declare (ignore dictionary))
  (reinitialize-instance
   clone
   :read-only-p (cleavir-ast:read-only-p original)))
