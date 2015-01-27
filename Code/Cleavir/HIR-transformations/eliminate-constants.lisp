(cl:in-package #:cleavir-hir-transformations)

;;;; When this transformation is invoked, constants that can be turned
;;;; into immediate values have already been removed.  We turn the
;;;; remaining constants into LOAD-TIME-VALUE-INPUTs so that we have a
;;;; uniform treatment of LOAD-TIME-VALUE and constants when the HIR
;;;; code is turned into MIR.

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
