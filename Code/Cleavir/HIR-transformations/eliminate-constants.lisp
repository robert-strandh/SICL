(cl:in-package #:cleavir-hir-transformations)

(defun eliminate-constants (initial-instruction)
  (traverse
   initial-instruction
   (lambda (instruction owner)
     (declare (ignore owner))
     (setf (cleavir-ir:inputs instruction)
	   (loop for input in (cleavir-ir:inputs instruction)
		 collect
		 (if (typep input 'cleavir-ir:constant-input)
		     (cleavir-ir:make-load-time-value-input
		      `(quote ,(cleavir-ir:value input)) t) 
		     input))))))
