(cl:in-package #:cleavir-lexical-depth)

;;;; An instruction A is a DESCENDANT of an instruction B if and only
;;;; if it is possible to reach A by starting at B and following
;;;; SUCCESSOR references.  As defined here, the set of DESCENDANTS of
;;;; an instruction includes the instruction itself.
;;;;
;;;; Recall that an ENTER-INSTRUCTION is the successor or no other
;;;; instruction.
;;;;
;;;; The OWNER of an instruction A is the outermost ENTER-INSTRUCTION
;;;; of all the ENTER-INSTRUCTIONs of which A is a descendant.  The
;;;; owner of a datum D is the outermost ENTER-INSTRUCTION of all the
;;;; owners of all the instructions using D.
;;;;
;;;; Each ENTER-INSTRUCTION A defines a PROCEDURE which is a the set
;;;; of all the instructions owned by A.  We extend the definition of
;;;; ownership so that a procedure P is the owner of some instruction
;;;; or datum X if an only if the unique ENTER-INSTRUCTION of P is the
;;;; owner of X. 
;;;;
;;;; A procedure P is a LEXICAL PARENT of a procedure Q if and only if
;;;; either some instruction in P is the direct successor of some
;;;; instruction in Q, or some instruction A in Q refers to a datum
;;;; owned by P. 
;;;;
;;;; The LEXICAL DEPTH of a procedure P is defined recursively as
;;;; follows:
;;;; 
;;;;   * If P has no lexical parent then its lexical depth is 0.
;;;;
;;;;   * Otherwise, the lexical depth of P is d+1, where d is the
;;;      maximum depth of any of its lexical parents. 

;;; Compute the owner of each instruction and each datum.  The return
;;; value is an EQ hash table mapping an instruction or a datum to its
;;; owner.
(defun compute-ownerships (enter-instruction)
  (let ((worklist (list enter-instruction))
	(result (make-hash-table :test #'eq)))
    (flet
	((process-function (enter-instruction)
	   (labels
	       ((traverse (instruction)
		  (when  (null (gethash instruction result))
		    (setf (gethash instruction result) enter-instruction)
		    (let ((data (append (cleavir-mir:inputs instruction)
					(cleavir-mir:outputs instruction))))
		      (loop for datum in data
			    do (when (null (gethash datum result))
				 (setf (gethash datum result)
				       enter-instruction))))
		    (when (typep instruction 'cleavir-mir:enclose-instruction)
		      (let ((code (cleavir-mir:code instruction)))
			(setf worklist (append worklist (list code)))))
		    (mapc #'traverse (cleavir-mir:successors instruction)))))
	     (traverse enter-instruction))))
      (loop until (null worklist)
	    do (process-function (pop worklist))))
    result))

(defun lexical-depths (enter-instruction)
  (let ((ownerships (compute-ownerships enter-instruction))
	(worklist (list enter-instruction))
	(depths (make-hash-table :test #'eq)))
    (flet
	((process-function (enter-instruction)
	   (setf (gethash enter-instruction depths) 0)
	   (let ((visited (make-hash-table :test #'eq)))
	     (labels
		 ((traverse (instruction)
		    (unless (gethash instruction visited)
		      (setf (gethash instruction visited) t)
		      (when (typep instruction 'cleavir-mir:enclose-instruction)
			(let ((code (cleavir-mir:code instruction)))
			  (setf worklist (append worklist (list code)))))
		      (loop with data = (append
					 (cleavir-mir:inputs instruction)
					 (cleavir-mir:outputs instruction))
			    with i-owner = (gethash instruction ownerships)
			    for datum in data
			    for d-owner = (gethash datum ownerships)
			    do (unless (eq d-owner i-owner)
				 (setf (gethash enter-instruction depths)
				       (max (gethash enter-instruction depths)
					    (1+ (gethash d-owner depths))))))
		      (loop with i-owner = (gethash instruction ownerships)
			    for successor in (cleavir-mir:successors instruction)
			    for s-owner = (gethash successor ownerships)
			    do (if (eq i-owner s-owner)
				   (traverse successor)
				   (setf (gethash enter-instruction depths)
					 (max (gethash enter-instruction depths)
					      (1+ (gethash s-owner depths)))))))))
	       (traverse enter-instruction)))))
      (loop until (null worklist)
	    do (process-function (pop worklist))))
    depths))
