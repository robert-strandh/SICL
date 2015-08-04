(cl:in-package #:cleavir-type-inference)

(defun compute-initial-work-list (initial-instruction)
  (let ((result '()))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (when (typep instruction 'cleavir-ir:enter-instruction)
	 (push instruction result)))
     initial-instruction)))
