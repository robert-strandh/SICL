(cl:in-package #:cleavir-remove-useless-instructions)

(defgeneric instruction-may-be-removed-p (instruction))

(defmethod instruction-may-be-removed-p (instruction)
  (and (= (length (cleavir-ir:successors instruction)) 1)
       (loop for output in (cleavir-ir:outputs instruction)
	     always (null (cleavir-ir:using-instructions output)))))

(defmethod instruction-may-be-removed-p
    ((instruction cleavir-ir:enter-instruction))
  nil)

(defmethod instruction-may-be-removed-p
    ((instruction cleavir-ir:unwind-instruction))
  nil)

(defmethod instruction-may-be-removed-p
    ((instruction cleavir-ir:funcall-instruction))
  nil)

(defmethod instruction-may-be-removed-p
    ((instruction cleavir-ir:return-instruction))
  nil)

(defmethod instruction-may-be-removed-p
    ((instruction cleavir-ir:multiple-value-call-instruction))
  nil)

(defmethod instruction-may-be-removed-p
    ((instruction cleavir-ir:rplaca-instruction))
  nil)

(defmethod instruction-may-be-removed-p
    ((instruction cleavir-ir:rplacd-instruction))
  nil)

(defun remove-useless-instructions (initial-instruction)
  (cleavir-meter:with-meter (m *remove-useless-instructions-meter*)
    (loop do (cleavir-ir:reinitialize-data initial-instruction)
	     (let ((useless-instructions '()))
	       (cleavir-ir:map-instructions-arbitrary-order
		(lambda (instruction)
		  (cleavir-meter:increment-size m)
		  (when (instruction-may-be-removed-p instruction)
		    (push instruction useless-instructions)))
		initial-instruction)
	       (when (null useless-instructions)
		 (loop-finish))
	       (mapc #'cleavir-ir:delete-instruction useless-instructions)))))
