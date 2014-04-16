(in-package #:cleavir-ssa-form)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute nodes where a PHI functions should be inserted for a
;;; single variable V.
;;;
;;; This function takes the following arguments:
;;;
;;;   * DOMINANCE-FRONTIERS.  The dominance frontiers of every node as
;;;     computed by the function CLEAVIR-DOMINANCE:DOMINANCE-FRONTIERS.
;;;
;;;   * LIVE-P. A function of one argument that takes a node and
;;;     returns true if and only if V is live at that node.  Client
;;;     code can pass (CONSTANTLY T) as an argument, in which case the
;;;     ordinary (not pruned) SSA form will be computed.
;;;
;;;   * NODES.  A set of nodes where V is being defined (i.e., its
;;;     value is modified)
;;;
;;; This function retuns a set (represented as a list) of nodes
;;; corresponding to the join ponts where phi functions should be
;;; inserted for V.
;;;
;;; The algorithm used is an adaptation of the algorithm in the 1991
;;; paper by Cytron et al, "Efficiently Computing Static Single
;;; Assignment Form and the Control Dependence Graph".  Their flags
;;; named Work is what is named PROCESSED-P in our algorithm.  We have
;;; no equivalent of their flags HasAlready; instead we just test
;;; whether the node is already a memmber of RESULT.  Our solution
;;; might have some extra cost associated with it if the number of PHI
;;; nodes for some variable should turn out to be very large.  Should
;;; that be the case, we might consider adding a second hash table
;;; with the same contents as RESULT.
;;;
;;; We include a modification of this algorithm as defined in in paper
;;; by Choi, Cytron and Ferranti, "Automatic Construction of Sparse
;;; Data Flow Evaluation Graphs" where nodes are included only if V is
;;; live there.  

(defparameter *ssa1-call-count* 0)
(defparameter *ssa1-run-time* 0)
(defparameter *ssa1-node-count-1* 0)
(defparameter *ssa1-node-count-2* 0)

(defun phi-function-nodes (dominance-frontiers live-p nodes)
  (let ((time (get-internal-run-time))
	(result '())
	(worklist nodes)
	(processed-p (make-hash-table :test #'eq)))
    (loop until (null worklist)
	  for x = (pop worklist)
	  for df = (cleavir-dominance:dominance-frontier dominance-frontiers x)
	  do (loop for y in df
		   do (when (and (not (member y result :test #'eq))
				 (funcall live-p y))
			(push y result)
			(unless (gethash y processed-p)
			  (setf (gethash y processed-p) t)
			  (push y worklist)))))
    (incf *ssa1-call-count*)
    (incf *ssa1-run-time* (- (get-internal-run-time) time))
    (incf *ssa1-node-count-1* (length nodes))
    (incf *ssa1-node-count-2* (length result))
    result))

