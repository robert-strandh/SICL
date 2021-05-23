(cl:in-package #:cleavir-basic-blocks)

;;; The owner of a basic block is either an ENTER instruction that
;;; represents the function to which the basic block belongs, or it is
;;; NIL if the basic block is outside the outermost ENTER instruction.
(defclass basic-block ()
  ((%first :accessor first-instruction :initarg :first-instruction)
   (%last :accessor last-instruction :initarg :last-instruction)
   (%owner :accessor owner :initarg :owner)
   (%successors :accessor successors :initarg :successors :initform nil)
   (%predecessors :accessor predecessors :initarg :predecessors :initform nil)))

;;;; Compute the basic blocks of a flow chart.
;;;;
;;;; To compute the basic blocks, we proceed in two steps:
;;;;
;;;;   * We first identify the LEADERS.  An instructions that fulfils
;;;;     at least one of the following conditions is a leader:
;;;;
;;;;     - It does not have a single predecessor.
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

;;; Return a list of basic blocks.
(defun basic-blocks (initial-instruction)
  (let ((leaders (make-hash-table :test #'eq)))
    (cleavir-ir:map-instructions-with-owner
     (lambda (instruction owner)
       (let ((preds (cleavir-ir:predecessors instruction)))
	 (when (or (/= (length preds) 1)
		   (/= (length (cleavir-ir:successors (first preds))) 1))
	   (setf (gethash instruction leaders)
                 (make-instance 'basic-block
                                :owner owner
                                :first-instruction instruction)))))
     initial-instruction)
    (flet ((leaderp (instruction)
	     (nth-value 1 (gethash instruction leaders))))
      (loop for first being each hash-key of leaders using (hash-value basic-block)
	    collect
            (loop with last = first
		  for successors = (cleavir-ir:successors last)
		  until (or (/= (length successors) 1)
			    (leaderp (first successors)))
		  do (setf last (first successors))
		  finally
                     (let ((basic-block (gethash first leaders)))
                       (setf (last-instruction basic-block) last)
                       (dolist (successor (cleavir-ir:successors last))
                         (pushnew (gethash successor leaders)
                                  (successors basic-block))
                         (pushnew basic-block
                                  (predecessors (gethash successor leaders))))
                       (return basic-block)))))))

;;; Compute a hash table mapping instructions to their containing basic block
(defun instruction-basic-blocks (basic-blocks)
  (let ((map (make-hash-table :test #'eq)))
    (dolist (basic-block basic-blocks)
      (map-basic-block-instructions
       (lambda (instruction)
         (setf (gethash instruction map) basic-block))
       basic-block))
    map))

(defun map-basic-block-instructions (function basic-block)
  (let ((first (first-instruction basic-block))
        (last (last-instruction basic-block)))
    (do ((instruction first (cleavir-ir:first-successor instruction)))
        ((eq instruction last) (funcall function instruction))
      (funcall function instruction))))

(defun containing-basic-block (instruction basic-blocks)
  (dolist (basic-block basic-blocks)
    (map-basic-block-instructions (lambda (i)
                                    (when (eq i instruction)
                                      (return basic-block)))
                                  basic-block)))
