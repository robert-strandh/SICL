(cl:in-package #:cleavir-hir-transformations)

(defun compute-nesting-depth (initial-instruction)
  (let ((result (make-hash-table :test #'eq)))
    (labels ((traverse (instruction depth)
	       (when (null (gethash instruction result))
		 (setf (gethash instruction result) depth)
		 (let ((successors (cleavir-ir:successors instruction)))
		   (typecase instruction
		     (cleavir-ir:unwind-instruction
		      (let ((invocation (cleavir-ir:invocation instruction)))
			(traverse (first successors)
				  (if (null invocation)
				      0
				      (gethash invocation result)))))
		     (cleavir-ir:enclose-instruction
		      (loop for successor in successors
			    do (traverse successor depth))
		      (traverse (cleavir-ir:code instruction) (1+ depth)))
		     (t
		      (loop for successor in successors
			    do (traverse successor depth))))))))
      (traverse initial-instruction 0))
    result))
			       
				    
