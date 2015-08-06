(cl:in-package #:cleavir-path-replication)

;;; Replicate all the instructions between DOMINATOR and INSTRUCTION.
;;; This transformation is accomplished by moving INSTRUCTION so that
;;; it precedes its current predecessors until it is precedes
;;; DOMINATOR.
;;;
;;; If DOMINATOR is not a dominator of INSTRUCTION, then this
;;; transformation will fail in some spectacular way.
;;;
;;; Also, if any of the instructions between DOMINATOR and INSTRUCTION
;;; writes a lexical location that is an input of INSTRUCTION, then
;;; this transformation will also fail.
(defun path-replication (instruction dominator)
  (loop with work-list = (list instruction)
	until (null work-list)
	do (let* ((instruction (pop work-list))
		  (predecessors (cleavir-ir:predecessors instruction)))
	     (assert (not (null predecessors)))
	     (cond ((> (length predecessors) 1)
		    (setf work-list
			  (append (replicate-instruction instruction)
				  work-list)))
		   ((eq (first predecessors) dominator)
		    ;; If the sole predecessor is DOMINATOR, then we
		    ;; move INSTRUCTION, and we are done.
		    (move-instruction instruction))
		   (t
		    ;; If the sole predecessor is some instruction
		    ;; other than DOMINATOR, then we move INSTRUCTION,
		    ;; but we put it back on WORK-LIST so that it is
		    ;; processed again.
		    (push instruction work-list)
		    (move-instruction instruction))))))
