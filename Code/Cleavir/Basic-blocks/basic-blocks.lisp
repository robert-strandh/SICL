(cl:in-package #:cleavir-basic-blocks)

;;;; Compute the basic blocks of a flow chart.
;;;;
;;;; To compute the basic blocks, we proceed in two steps:
;;;;
;;;;   * We first identify the LEADERS.  An instructions that fulfils
;;;;     at least one of the following conditions is a leader:
;;;;
;;;;     - It does not have a single predecessor.
;;;;
;;;;     - It has an UNWIND-INSTRUCTION as its predecessor.
;;;;
;;;;     - It has a predecessor with more than one successor.
;;;;
;;;;     Every leader defines exactly one basic block.
;;;;
;;;;   * Next, for each leader, we initialize a basic block consisting
;;;;     of that leader as its first instruction AND its last
;;;;     instruction.  We then extend the basic block as long as the
;;;;     last instruction has a single successor AND that successor is
;;;;     not a leader.

;;; Return a list of basic blocks.  Each basic block is represented as
;;; a list of three elements: the first instruction of the basic
;;; block, the last instruction of the basic block, and the OWNER of
;;; the basic block.  The owner of a basic block is either an ENTER
;;; instruction that represents the function to which the basic block
;;; belongs, or it is NIL if the basic block is outside the outermost
;;; ENTER instruction.
(defun basic-blocks (start-node)
  (let ((leaders (make-hash-table :test #'eq)))
    (flet ((successors (node)
	     (cleavir-ir:successors node))
	   (predecessors (node)
	     (cleavir-ir:predecessors node)))
      (let ((table (make-hash-table :test #'eq)))
	(labels ((traverse (node owner)
		   (when  (null (gethash node table))
		     (setf (gethash node table) t)
		     (when (typep node 'cleavir-ir:enter-instruction)
		       (setf owner node))
		     (let ((preds (predecessors node)))
		       (when (or (/= (length preds) 1)
				 (/= (length (successors (first preds))) 1)
				 (typep (first preds)
					'cleavir-ir:unwind-instruction))
			 (setf (gethash node leaders) owner)))
		     (let ((succs (successors node)))
		       (if (typep node 'cleavir-ir:unwind-instruction)
			   (traverse (first succs)
				     (cleavir-ir:invocation node))
			   (loop for successor in succs
				 do (traverse successor owner))))
		     (when (typep node 'cleavir-ir:enclose-instruction)
		       (traverse (cleavir-ir:code node) owner)))))
	  (traverse start-node nil)))
      (flet ((leaderp (node)
	       (nth-value 1 (gethash node leaders))))
	(loop for first being each hash-key of leaders using (hash-value owner)
	      collect (loop with last = first
			    for successors = (successors last)
			    until (or (/= (length successors) 1)
				      (leaderp (first successors)))
			    do (setf last (first successors))
			    finally (return (list first last owner))))))))
