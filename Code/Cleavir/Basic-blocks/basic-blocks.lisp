(cl:in-package #:cleavir-basic-blocks)

;;;; Compute the basic blocks of a flow chart. 

;;; Return a list of basic blocks.  Each basic block is represented as
;;; a CONS of the FIRST and the LAST nodes of the block. 
(defun basic-blocks (start-node successor-fun)
  (let ((predecessor-fun (cleavir-utilities:predecessor-function
			  start-node successor-fun))
	(leaders (make-hash-table :test #'eq)))
    (flet ((successors (node)
	     (funcall successor-fun node))
	   (predecessors (node)
	     (funcall predecessor-fun node)))
      (let ((table (make-hash-table :test #'eq)))
	(labels ((traverse (node)
		   (unless (gethash node table)
		     (setf (gethash node table) t)
		     (when (/= (length (predecessors node)) 1)
		       (setf (gethash node leaders) t))
		     (mapc #'traverse (successors node)))))
	  (traverse start-node)))
      (loop for first being each hash-key of leaders
	    collect (loop for last = first then (car (successors last))
			  until (or (/= (length (successors last)) 1)
				    (gethash (car (successors last)) leaders))
			  finally (return (cons first last)))))))
			  
