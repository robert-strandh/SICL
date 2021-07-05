(cl:in-package #:cleavir-ssa-form)

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
;;; This function returns a set (represented as a list) of nodes
;;; corresponding to the join points where phi functions should be
;;; inserted for V.
;;;
;;; The algorithm used is an adaptation of the algorithm in the 1991
;;; paper by Cytron et al, "Efficiently Computing Static Single
;;; Assignment Form and the Control Dependence Graph".  Their flags
;;; named Work is what is named PROCESSED-P in our algorithm.  We have
;;; no equivalent of their flags HasAlready; instead we just test
;;; whether the node is already a member of RESULT.  Our solution
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute PHI instructions for a single variable V.
;;;
;;; The algorithm is an adaptation of the algorithm in the 1991 paper
;;; by Cytron et al, "Efficiently Computing Static Single Assignment
;;; Form and the Control Dependence Graph".
;;;
;;; First of all, in this algorithm, we are using individual
;;; instructions rather than basic blocks.  Therefore, what is a loop
;;; over all "statements" in the paper is no longer a loop here.
;;;
;;; Second, this algorithm is valid for a single variable.  For that
;;; reason, we do not have a stack for each variable.
;;;
;;; Third, what is an explicit stack for each variable in the paper is
;;; a stack handled by the call stack as an argument here.  For that
;;; reason, we no not need explicit pops.
;;;
;;; Fourth, the paper pushes the variable each time the variable is
;;; encountered on a left hand side.  Then as many pops are executed
;;; afterwards.  These pushes and pops are not needed, because the
;;; intermediate variables are never at the top of the stack.  But
;;; since we have a basic block per instruction, we actually DO
;;; (implicitly) push all those intermediate variables. 
;;;
;;; TOP-NODE is the topmost (in the dominance tree) node in which the
;;; variable is mentioned (and therefore assigned to).  PHI-NODES are
;;; the nodes returned by the function PHI-FUNCTION-NODES above.
;;; ORIGINAL-VARIABLE is the variable to be processed.  LIVE-P is a
;;; function that takes a node and returns true if and only if the
;;; variable is live in that node.  DOMINEES is a function that takes
;;; a node N and returns a list of nodes that are immediately
;;; dominated by N.

(defparameter *ssa2-call-count* 0)
(defparameter *ssa2-run-time* 0)

(defun phi-instructions (top-node phi-nodes original-variable live-p dominees)
  (let ((time (get-internal-run-time))
        (funs (loop for node in phi-nodes
                    for count = (length (cleavir-ir:predecessors node))
                    collect (cons node (make-list (1+ count)))))
        (name (cleavir-ir:name original-variable)))
    (labels
        ((traverse (node variable)
           (if (not (funcall live-p node))
               nil
               (let ((phi-fun (rest (assoc node funs :test #'eq))))
                 (setf (cleavir-ir:inputs node)
                       (substitute variable original-variable
                                   (cleavir-ir:inputs node)))
                 (unless (null phi-fun)
                   (setf variable (cleavir-ir:make-lexical-location name))
                   (setf (first phi-fun) variable))
                 (loop for succ in (cleavir-ir:successors node)
                       for phi-fun = (rest (assoc succ funs :test #'eq))
                       for pos = (position node (cleavir-ir:predecessors succ))
                       do (unless (null phi-fun)
                            (setf (first (nthcdr pos (rest phi-fun)))
                                  variable)))
                 (loop for dominee in (funcall dominees node)
                       do (traverse dominee variable))))))
      (traverse top-node original-variable))
    (loop for (node output . inputs) in funs
          collect (let ((cleavir-ir:*policy*
                          (cleavir-ir:policy node)))
                    (cleavir-ir:make-phi-instruction inputs output node))
          finally (incf *ssa2-call-count*)
                  (incf *ssa2-run-time* (- (get-internal-run-time) time)))))
