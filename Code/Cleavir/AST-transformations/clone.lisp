(cl:in-package #:cleavir-ast-transformations)

(defun clone-ast (ast)
  (let ((dictionary (make-hash-table :test #'eq)))
    (labels
	((traverse (node)
	   (when (null (gethash node dictionary))
	     (setf (gethash node dictionary)
		   (apply #'make-instance (class-of node)
			  (loop for (keyword reader)
				  in (cleavir-io:save-info node)
				collect keyword
				collect (funcall reader node))))
	     (mapc #'traverse (cleavir-ast:children node)))))
      (traverse ast))
    (maphash
     (lambda (key value)
       (apply #'reinitialize-instance
	      value
	      (loop for (keyword reader) in (cleavir-io:save-info key)
		    for new = (gethash (funcall reader key) dictionary)
		    unless (null new)
		      collect keyword
		      and collect new)))
     dictionary)
    (gethash ast dictionary)))
