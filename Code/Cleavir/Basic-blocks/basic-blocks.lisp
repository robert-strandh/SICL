(cl:in-package #:cleavir-basic-blocks)

;;;; Compute the basic blocks of a flow chart. 
;;;;
;;;; To compute the basic blocks, we proceed in two steps:
;;;; 
;;;;   * We first identify the LEADERS.  A leader is an instruction
;;;;     that either does not have exactly one predecessor, or whose
;;;;     predecessor has more than one successor.  A leader
;;;;     corresponds to exactly one basic block.
;;;; 
;;;;   * Next, for each leader, we initialize a basic block consisting
;;;;     of that leader as its first instruction AND its last
;;;;     instruction.  We then extend the basic block as long as the
;;;;     last instruction has a single successor AND that successor is
;;;;     not a leader.

;;; Return a list of basic blocks.  Each basic block is represented as
;;; a CONS of the FIRST and the LAST nodes of the block. 
(defun basic-blocks (start-node)
  (let ((leaders (make-hash-table :test #'eq)))
    (flet ((successors (node)
	     (cleavir-ir:successors node))
	   (predecessors (node)
	     (cleavir-ir:predecessors node)))
      (let ((table (make-hash-table :test #'eq)))
	(labels ((traverse (node)
		   (unless (gethash node table)
		     (setf (gethash node table) t)
		     (let ((preds (predecessors node)))
		       (when (or (/= (length preds) 1)
				 (> (length (successors (car preds))) 1))
			 (setf (gethash node leaders) t))
		       (mapc #'traverse (successors node))))))
	  (traverse start-node)))
      (loop for first being each hash-key of leaders
	    collect (loop for last = first then (car (successors last))
			  until (or (/= (length (successors last)) 1)
				    (gethash (car (successors last)) leaders))
			  finally (return (cons first last)))))))
			  
