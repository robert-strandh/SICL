(cl:in-package #:cleavir-boolean-elimination)

;;; We search for EQ-INSTRUCTIONs where one of the inputs is a
;;; LOAD-TIME-VALUE-INPUT with a constant form of NIL, meaning that
;;; the instruction tests its other input as a Boolean.  We return a
;;; list of such instructions in the program.
(defun find-boolean-tests (initial-instruction)
  (let ((result '()))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (when (typep instruction 'cleavir-ir:eq-instruction)
	 (destructuring-bind (i1 i2) (cleavir-ir:inputs instruction)
	   (when (or (boolean-input-p i1 nil)
		     (boolean-input-p i2 nil))
	     (push instruction result)))))
     initial-instruction)
    result))
