(cl:in-package #:cleavir-hir-transformations)

;;; Compute the NESTING DEPTH of each instruction of a program.
;;; Return the result as an EQ hash table, mapping each instruction to
;;; its depth.
;;;
;;; The nesting depth is defined recursively as follows:
;;;
;;;  * The nesting depth of the initial instruction (which is always
;;;    an ENTER-INSTRUCTION) is 0.
;;;
;;;  * The nesting depth of the unique successor of an UNWIND
;;;    instruction is the nesting depth of the INVOCATION (which is
;;;    always an ENTER-INSTRUCTION) of the UNWIND instruction.
;;;
;;;  * The nesting depth of a successor of an instruction I other than
;;;    an UNWIND instruction is the same as the nesting depth of I.
;;;
;;;  * For an ENCLOSE instruction E, the nesting depth of its CODE
;;;    (which is an ENTER instruction) is d+1, where d is the nesting
;;;    depth of E.
;;;
;;; Since an instruction is exclusively either the initial
;;; instruction, the CODE of an ENCLOSE instruction, or the successor
;;; of some other instruction, this definition covers all possible
;;; cases with no possible ambiguity.

(defun compute-nesting-depth (initial-instruction)
  (let ((result (make-hash-table :test #'eq)))
    ;; As required by the definition of nesting depth, we set the
    ;; nesting depth of the initial instruction to 0.
    (setf (gethash initial-instruction result) 0)
    (labels ((traverse (instruction depth)
	       (when (null (gethash instruction result))
		 (setf (gethash instruction result) depth)
		 (let ((successors (cleavir-ir:successors instruction)))
		   (typecase instruction
		     (cleavir-ir:unwind-instruction
		      (let ((invocation (cleavir-ir:invocation instruction)))
			(traverse (first successors)
				  (gethash invocation result))))
		     (cleavir-ir:enclose-instruction
		      (loop for successor in successors
			    do (traverse successor depth))
		      (traverse (cleavir-ir:code instruction) (1+ depth)))
		     (t
		      (loop for successor in successors
			    do (traverse successor depth))))))))
      (traverse initial-instruction 0))
    result))
			       
				    
