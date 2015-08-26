(cl:in-package #:cleavir-boolean-elimination)

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
