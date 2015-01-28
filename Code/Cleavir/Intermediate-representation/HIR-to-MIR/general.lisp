(cl:in-package #:cleavir-ir)

(defgeneric specialize (instruction implementation processor os))

(defun hir-to-mir (initial-instruction implementation processor os)
  ;; Gather up all instructions in a table
  (let ((table (make-hash-table :test #'eq)))
    (labels ((traverse (instruction)
	       (unless (gethash instruction table)
		 (setf (gethash instruction table) t)
		 (loop for succ in (cleavir-ir:successors instruction)
		       do (traverse succ))
		 (when (typep instruction 'cleavir-ir:enclose-instruction)
		   (traverse (cleavir-ir:code instruction))))))
      (traverse initial-instruction))
    ;; Appply specialize to all instructions in the table.
    (loop for instruction being each hash-key of table
	  do (specialize instruction implementation processor os))))

(defmethod specialize :around (instruction implementation processor os)
  (declare (ignorable implementation processor os))
  (let ((replacement (call-next-method)))
    (unless (eq instruction replacement)
      (setf (cleavir-ir:predecessors replacement)
	    (cleavir-ir:predecessors instruction))
      (loop for predecessor in (cleavir-ir:predecessors instruction)
	    do (nsubstitute replacement instruction
			    (cleavir-ir:successors predecessor))))))
