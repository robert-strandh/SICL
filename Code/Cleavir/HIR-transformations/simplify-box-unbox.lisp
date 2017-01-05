(cl:in-package #:cleavir-hir-transformations)

;;;; We simplify boxing and unboxing in the following way: For each
;;;; unboxing instruction U, we check how the input was generated.  If
;;;; it was generated from a corresponding boxing instruction B, we
;;;; replace U with an assignment.  The assignment has the same output
;;;; as U, and the input of B.
;;;;
;;;; It is unlikely that there will be any pairs of an unboxing
;;;; instruction generating input for a boxing instruction, because
;;;; when there is some unboxing operation, it is in order to perform
;;;; some operation on the unboxed value, or to store it in some
;;;; specialized array.
;;;;
;;;; When all simplifications have been made, it is possible that
;;;; there are boxing instructions that generate outputs that are
;;;; never used.  Those boxing instructions are then deleted.

(defun maybe-remove-unbox (unbox)
  (let* ((input (first (cleavir-ir:inputs unbox)))
	 (defs (cleavir-ir:defining-instructions input)))
    (when (and (= (length defs) 1)
	       (typep (first defs) 'cleavir-ir:box-instruction))
      (change-class unbox
		    'cleavir-ir:assignment-instruction
		    ;; copy the list?
		    :inputs (cleavir-ir:inputs (first defs))))))

(defun maybe-remove-box (box)
  (let* ((output (first (cleavir-ir:outputs box)))
	 (users (cleavir-ir:using-instructions output)))
    (describe output)
    (when (null users)
      (cleavir-ir:delete-instruction box))))

(defun simplify-boxes (instruction)
  (mapc #'maybe-remove-unbox
	(cleavir-ir:instructions-of-type
	 instruction 'cleavir-ir:unbox-instruction))
  ;; this could be done more efficiently (in maybe-remove-unbox)
  (cleavir-ir:reinitialize-data instruction)
  (mapc #'maybe-remove-box
	(cleavir-ir:instructions-of-type
	 instruction 'cleavir-ir:box-instruction))
  instruction)


;;  LocalWords:  unboxing unboxed

