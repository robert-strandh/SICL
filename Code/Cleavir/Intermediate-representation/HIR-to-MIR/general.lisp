(cl:in-package #:cleavir-ir)

(defgeneric specialize (instruction implementation processor os))

(defun hir-to-mir (initial-instruction implementation processor os)
  (let ((all-instructions '()))
    ;; Gather up all instructions in a list.
    (map-instructions
     (lambda (instruction)
       (push instruction all-instructions))
     initial-instruction)
    ;; Appply specialize to all instructions in the list.
    (loop for instruction in all-instructions
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
