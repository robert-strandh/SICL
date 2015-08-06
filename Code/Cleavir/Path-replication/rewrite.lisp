(cl:in-package #:cleavir-path-replication)

;;; This rewrite rule is used as a preparation for moving INSTRUCTION
;;; past its predecessors, because in order to do that, INSTRUCTION
;;; must have a single predecessor.  This function replicates
;;; INSTRUCTION for each predecessor P.  It does that by creating a
;;; replica RP of INSTRUCTION, then it replaces INSTRUCTION with RP in
;;; the successors of P.  Finally, it replaces INSTRUCTION by all the
;;; RPs in each successor of INSTRUCTION.
;;;
;;; We return a list of the copies that were created.
;;;
;;; Notice that we do NOT update the defining and using instructions
;;; of the inputs and outputs of INSTRUCTION.  After this rewrite has
;;; been accomplished, this information must be updated explicitly if
;;; required.
(defun replicate-instruction (instruction)
  (let ((copies
	  (loop for predecessor in (cleavir-ir:predecessors instruction)
		for successors = (cleavir-ir:successors predecessor)
		for copy = (make-instance (class-of instruction)
			     :predecessors (list predecessor)
			     :successors (cleavir-ir:successors instruction)
			     :inputs (cleavir-ir:inputs instruction)
			     :outputs (cleavir-ir:outputs instruction))
		do (nsubstitute copy instruction successors)
		collect copy)))
    (loop for successor in (cleavir-ir:successors instruction)
	  for predecessors = (cleavir-ir:predecessors successor)
	  do (setf (cleavir-ir:predecessors successor)
		   (loop for predecessor in predecessors
			 if (eq predecessor instruction)
			   append copies
			 else
			   collect predecessor)))
    copies))

;;; This rewrite rule moves INSTRUCTION so that it precedes its
;;; current predecessor.  INSTRUCTION must have a single predecessor.
;;; It accomplishes the rewrite rule by replicating its current
;;; predecessor between itself and each of its current successors.
(defun move-instruction (instruction)
  (assert (= (length (cleavir-ir:predecessors instruction)) 1))
  (loop with predecessor = (first (cleavir-ir:predecessors instruction))
	for successors = (cleavir-ir:successors predecessor)
	for remaining on (cleavir-ir:successors instruction)
	for successor = (first remaining)
	for copy = (make-instance (class-of predecessor)
		     :predecessors (list instruction)
		     :successors (substitute successor instruction successors)
		     :inputs (cleavir-ir:inputs predecessor)
		     :outputs (cleavir-ir:outputs predecessor))
	do (setf (first remaining) copy)
	   (nsubstitute copy instruction (cleavir-ir:predecessors successor))
	   (loop for pred in (cleavir-ir:predecessors predecessor)
		 do (nsubstitute instruction predecessor
				 (cleavir-ir:successors pred)))))
