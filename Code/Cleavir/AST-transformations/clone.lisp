(cl:in-package #:cleavir-ast-transformations)

;;; Cloning an AST is done in two steps.
;;;
;;; In step one, we create a dictionary mapping every node in the
;;; original AST to a cloned node.  The cloned node is created by
;;; calling MAKE-INSTANCE on the class of the original node.  The
;;; initialization arguments passed to MAKE-INSTANCE are obtained from
;;; the SAVE-INFO of the original node, using the same keys and the
;;; same values.  As a result, the slots of the coned node contain
;;; original AST nodes, but also non-AST information such as names,
;;; etc.
;;;
;;; In step two, we iterate over the dictionary entries.  For each
;;; entry, we reinitialize a cloned node by calling
;;; REINITIALIZE-INSTANCE on it.  As with step one, we compute the
;;; initialization arguments to REINITIALIZE-INSTANCE from the
;;; SAVE-INFO, but this time we keep only slot values that are AST
;;; nodes.  Furthermore, we replace each such AST node with the
;;; mapping computed in step one.
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
