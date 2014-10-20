(cl:in-package #:cleavir-ir)

(defvar *table*)

(defgeneric specialize (instruction implementation processor os))

(defun hir-to-mir (initial-instruction implementation processor os)
  (let ((*table* (make-hash-table :test #'eq)))
    (specialize initial-instruction implementation processor os)))

(defmethod specialize :around (instruction implementation processor os)
  (declare (ignorable implementation processor os))
  (unless (gethash instruction *table*)
    (setf (gethash instruction *table*) t)
    (let ((replacement (call-next-method)))
      (unless (eq instruction replacement)
	(setf (cleavir-ir:predecessors replacement)
	      (cleavir-ir:predecessors instruction))
	(loop for predecessor in (cleavir-ir:predecessors instruction)
	      do (nsubstitute replacement instruction
			      (cleavir-ir:successors predecessor)))))))
