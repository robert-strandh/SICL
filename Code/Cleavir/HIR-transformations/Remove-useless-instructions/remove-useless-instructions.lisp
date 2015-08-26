(cl:in-package #:cleavir-remove-useless-instructions)

(defgeneric instruction-may-be-removed-p (instruction))

(defmethod instruction-may-be-removed-p (instruction)
  (and (= (length (cleavir-ir:successors instruction)) 1)
       (loop for output in (cleavir-ir:outputs instruction)
	     always (null (cleavir-ir:using-instructions output)))))

(defun remove-useless-instructions (initial-instruction)
  (cleavir-ir:reinitialize-data initial-instruction)
  (let ((useless-instructions '()))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (when (and (= (length (cleavir-ir:successors instruction)) 1)
		  (not (typep instruction 'cleavir-ir:funcall-instruction))
		  (loop for output in (cleavir-ir:outputs instruction)
			always (null (cleavir-ir:using-instructions output))))
	 (push instruction useless-instructions)))
     initial-instruction)
    (mapc #'cleavir-ir:delete-instruction useless-instructions)))
