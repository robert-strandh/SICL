(in-package #:cleavir-ssa-form)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute nodes where a PHI functions should be inserted. 
;;;
;;; Given the dominance frontiers as computed by the function
;;; CLEAVIR-DOMINANCE:DOMINANCE-FRONTIERS and set S of nodes, return a
;;; set PHI of nodes corresponding to the join ponts where phi
;;; functions should be inserted for a variable that is defined in the
;;; nodes in S.
;;;
;;; This function returns nodes where variables might be dead.  It is
;;; up to client code to eliminate such nodes if needed.
;;;
;;; The algorithm used is an adaptation of the algorithm in Cytron et
;;; al, "Efficiently Computing Static Single Assignment Form and the
;;; Control Dependence Graph".  Their flags named Work is what is
;;; named PROCESSED-P in our algorithm.  We have no equivalent of
;;; their flags HasAlready; instead we just test whether the node is
;;; already a memmber of RESULT.  Our solution might have some extra
;;; cost associated with it if the number of PHI nodes for some
;;; variable should turn out to be very large.  Should that be the
;;; case, we might consider adding a second hash table with the same
;;; contents as RESULT. 

(defparameter *ssa1-call-count* 0)
(defparameter *ssa1-run-time* 0)
(defparameter *ssa1-node-count-1* 0)
(defparameter *ssa1-node-count-2* 0)

(defun phi-function-nodes (dominance-frontiers nodes)
  (let ((time (get-internal-run-time))
	(result '())
	(worklist nodes)
	(processed-p (make-hash-table :test #'eq)))
    (loop until (null worklist)
	  for x = (pop worklist)
	  for df = (cleavir-dominance:dominance-frontier dominance-frontiers x)
	  do (loop for y in df
		   do (unless (member y result :test #'eq)
			(push y result)
			(unless (gethash y processed-p)
			  (setf (gethash y processed-p) t)
			  (push y worklist)))))
    (incf *ssa1-call-count*)
    (incf *ssa1-run-time* (- (get-internal-run-time) time))
    (incf *ssa1-node-count-1* (length nodes))
    (incf *ssa1-node-count-2* (length result))
    result))

