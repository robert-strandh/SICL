(in-package #:cleavir-ssa-form)

;;; Given the dominance frontiers as computed by the function
;;; CLEAVIR-DOMINANCE:DOMINANCE-FRONTIERS and set S of nodes, return a
;;; set PHI of nodes corresponding to the join ponts where phi
;;; functions should be inserted for a variable that is defined in the
;;; nodes in S.
;;;
;;; This function returns nodes where variables might be dead.  It is
;;; up to client code to eliminate such nodes if needed.
(defun phi-function-nodes (dominance-frontiers nodes)
  (let ((result '())
	(worklist nodes)
	(processed-p (make-hash-table :test #'eq)))
    (loop until (null worklist)
	  for x = (pop worklist)
	  do (loop for y in (cleavir-dominance:dominance-frontier
			     dominance-frontiers x)
		   do (unless (member y result :test #'eq)
			(push y result)
			(unless (gethash y processed-p)
			  (setf (gethash y processed-p) t)
			  (push y worklist)))))
    result))
