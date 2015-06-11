(cl:in-package #:cleavir-hir-transformations)

(defun eliminate-superfluous-temporaries (initial-instruction)
  ;; Make sure we are working with an up-to-date graph, both with
  ;; respect to instructions and with respect to data.
  (cleavir-ir:set-predecessors initial-instruction)
  (cleavir-ir:reinitialize-data initial-instruction)
  (let (;; This hash table contains ASSIGNMENT-INSTRUCTIONs that can
	;; be eliminated because they assign to a superfluous
	;; DYNAMIC-LEXICAL-LOCATION.
	(superfluous-instructions (make-hash-table :test #'eq))
	;; This hash table contains DYNAMICAL-LEXICAL-LOCATIONs each
	;; of which is essential to keep because it is going to be
	;; used in place of some superfluous
	;; DYNAMICAL-LEXICAL-LOCATION.
	(essential-locations (make-hash-table :test #'eq))
	;; This hash table contains DYNAMICAL-LEXICAL-LOCATIONs each
	;; of which is superfluous because it is being assigned to
	;; from some essential DYNAMICAL-LEXICAL-LOCATION.
	(superfluous-locations (make-hash-table :test #'eq)))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (when (and (typep instruction
			 'cleavir-ir:assignment-instruction)
		  (let ((input (car (cleavir-ir:inputs instruction)))
			(output (car (cleavir-ir:outputs instruction))))
		    (and (typep input 'cleavir-ir:dynamic-lexical-location)
			 (= (length (cleavir-ir:defining-instructions input)) 1)
			 (not (gethash input superfluous-locations))
			 (typep output 'cleavir-ir:dynamic-lexical-location)
			 (= (length (cleavir-ir:using-instructions output)) 1)
			 (not (gethash output essential-locations)))))
	 (setf (gethash instruction superfluous-instructions) t)
	 (setf (gethash (car (cleavir-ir:inputs instruction)) essential-locations) t)
	 (setf (gethash (car (cleavir-ir:outputs instruction)) superfluous-locations) t)))
     initial-instruction)
    (maphash (lambda (instruction value)
	       (declare (ignore value))
	       (let* ((input (car (cleavir-ir:inputs instruction)))
		      (output (car (cleavir-ir:outputs instruction)))
		      (using-instruction (car (cleavir-ir:using-instructions output))))
		 (nsubstitute input output (cleavir-ir:inputs using-instruction))
		 (setf (car (cleavir-ir:using-instructions input)) using-instruction)
		 (cleavir-ir:delete-instruction instruction)))
	     superfluous-instructions)
    ;; Return the number of instructions that were eliminated.
    (hash-table-size superfluous-instructions)))
