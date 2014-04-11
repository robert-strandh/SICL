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

;;; Compute the owner of each instruction and each datum.  The owner
;;; of an instruction I is the outermost ENTER-INSTRUCTION of all the
;;; ENTER-INSTRUCTIONs which I is a descendant.  The owner of a datum
;;; D is the outermost ENTER-INSTRUCTION of all the owners of all the
;;; instructions using D.  The return value is an EQ hash table
;;; mapping an instruction or a datum to its owner.
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
