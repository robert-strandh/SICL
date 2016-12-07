(cl:in-package #:cleavir-hir-transformations)

(defun coalesce-load-time-values (initial-instruction)
  (check-type initial-instruction cleavir-ir:top-level-enter-instruction)
  (let ((table (make-hash-table :test #'equal))
	(cleavir-ir:*policy* (cleavir-ir:policy initial-instruction)))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (loop for input in (cleavir-ir:inputs instruction)
	     do (when (and (typep input 'cleavir-ir:load-time-value-input)
			   (cleavir-ir:read-only-p input))
		  (push (cons instruction input)
			(gethash (cleavir-ir:form input) table)))))
     initial-instruction)
    (loop for inputs being each hash-value of table
	  do (when (>= (length inputs) 2)
	       (let ((new-lexical (cleavir-ir:new-temporary))
		     (input (cdr (first inputs))))
		 (cleavir-ir:insert-instruction-after
		  (cleavir-ir:make-assignment-instruction input new-lexical)
		  initial-instruction)
		 (loop for (instruction . input) in inputs
		       do (nsubstitute new-lexical input
				       (cleavir-ir:inputs instruction)
				       :test #'eq)))))))
