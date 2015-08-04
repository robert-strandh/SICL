(cl:in-package #:cleavir-type-inference)

(defun compute-initial-work-list (initial-instruction)
  (let ((result '()))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (when (typep instruction 'cleavir-ir:enter-instruction)
	 (push instruction result)))
     initial-instruction)))

(defun compute-initial-dictionary (initial-instruction)
  (flet ((variable-p (datum)
	   (or (typep datum 'cleavir-ir:lexical-location)
	       (typep datum 'cleavir-ir:values-location))))
    (let ((liveness (cleavir-liveness:liveness
		     initial-instruction
		     #'cleavir-ir:successors
		     #'cleavir-ir:predecessors
		     #'cleavir-ir:inputs
		     #'cleavir-ir:outputs
		     #'variable-p))
	  (result (make-hash-table :test #'equal)))
      (cleavir-ir:map-instructions-arbitrary-order
       (lambda (instruction)
	 (loop for predecessor in (cleavir-ir:predecessors instruction)
	       for key = (cons predecessor instruction)
	       for live = (cleavir-liveness:live-before liveness instruction)
	       do (loop for var in live
			do (push (cons var t)
				 (gethash (cons predecessor instruction)
					  result)))))
       initial-instruction)
      result)))
