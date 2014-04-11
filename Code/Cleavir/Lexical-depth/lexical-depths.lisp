(cl:in-package #:cleavir-lexical-depth)

;;;; As defined here, the set of DESCENDANTS of an instruction
;;;; includes the instruction itself.

;;; Given an ENTER-INSTRUCTION, call FUNCTION once for each descendant
;;; of ENTER-INSTRUCTION.
(defun map-instructions (function enter-instruction)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((traverse (instruction)
	       (unless (gethash instruction table)
		 (setf (gethash instruction table) t)
		 (funcall function instruction)
		 (mapc #'traverse (cleavir-mir:successors instruction)))))
      (traverse enter-instruction))))

;;; Compute the owner of each instruction.  The owner of an
;;; instruction I is the outermost ENTER-INSTRUCTION of all the
;;; ENTER-INSTRUCTIONs which I is a descendant.  The return value is
;;; an EQ hash table mapping instructions to their owners.
(defun compute-instruction-ownerships (enter-instruction)
  (let ((worklist (list enter-instruction))
	(result (make-hash-table :test #'eq)))
    (flet ((process-function (enter-instruction)
	     (map-instructions
	      (lambda (instruction)
		(unless (gethash instruction result)
		  (setf (gethash instruction result) enter-instruction))
		(when (and (not (eq instruction enter-instruction))
			   (typep instruction 'cleavir-mir:enter-instruction))
		  (setf worklist (append worklist (list instruction)))))
	      enter-instruction)))
      (loop until (null worklist)
	    do (process-function (pop worklist))))
    result))
