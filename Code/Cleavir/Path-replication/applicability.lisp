(cl:in-package #:cleavir-path-replication)

(defun transformation-applicable-p (instruction putative-dominator)
  (when (eq instruction putative-dominator)
    (return-from transformation-applicable-p nil))
  (let ((table (make-hash-table :test #'eq))
	(work-list (list instruction))
	(inputs (cleavir-ir:inputs instruction)))
    (loop until (null work-list)
	  do (let* ((inst (pop work-list))
		    (predecessors (cleavir-ir:predecessors inst)))
	       (when (null predecessors)
		 ;; We have reached an instruction with no
		 ;; predecessors without running into the putative
		 ;; dominator.  This means that the putative dominator
		 ;; is NOT a dominator if INSTRUCTION.
		 (return-from transformation-applicable-p nil))
	       (setf (gethash inst table) t)
	       (loop for predecessor in predecessors
		     for outputs = (cleavir-ir:outputs predecessor)
		     for common = (intersection inputs outputs :test #'eq)
		     do (cond ((gethash predecessor table)
			       ;; We have already processed this
			       ;; instruction, so we do nothing.
			       nil)
			      ((not (null common))
			       ;; The predecessor writes a variable
			       ;; that is read by INSTRUCTION.  In
			       ;; that case, the transformation is not
			       ;; applicable.
			       (return-from transformation-applicable-p nil))
			      ((eq predecessor putative-dominator)
			       ;; The predecessor is the putative
			       ;; dominator, so at least as far as
			       ;; this particular predecessor is
			       ;; concerned, the putative dominator is
			       ;; indeed a dominator of INSTRUCTION.
			       ;; Since we are done with this
			       ;; particular item of the work list, we
			       ;; do nothing.
			       nil)
			      (t
			       ;; When we come here, we know that the
			       ;; predecessor does not write any
			       ;; variable that is read by
			       ;; INSTRUCTION, and that the
			       ;; predecessor is not the putative
			       ;; dominator.  We must therefore add
			       ;; the predecessor to the work list so
			       ;; that it can be processed the next
			       ;; time around.
			       (push predecessor work-list)))))
	  finally ;; We have reached the putative dominator in every path
		  ;; preceding INSTRUCTION and e
		  (return t))))
		     
	       
    
