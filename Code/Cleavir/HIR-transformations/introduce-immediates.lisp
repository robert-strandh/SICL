(cl:in-package #:cleavir-hir-transformations)

;;; Client code must provide one or more methods on this generic
;;; function that either return the first argument without
;;; modification, or that return an integer that represents a raw
;;; immediate input on the platform represented by the last three
;;; arguments.
(defgeneric introduce-immediate (constant implementation processor os))


(defun introduce-immediates (initial-instruction implementation processor os)
  (traverse
   initial-instruction
   (lambda (instruction owner)
     (declare (ignore owner))
     (setf (cleavir-ir:inputs instruction)
	   (loop for input in (cleavir-ir:inputs instruction)
		 collect
		 (if (typep input 'cleavir-ir:constant-input)
		     (let ((replacement (introduce-immediate input
							     implementation
							     processor
							     os)))
		       (if (eq input replacement)
			   input
			   (cleavir-ir:make-immediate-input replacement)))))))))
