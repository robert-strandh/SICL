(cl:in-package #:cleavir-hir-transformations)

;;;; Eliminating superfluous temporaries is a bit tricky.  We can not
;;;; guarantee that this function works as expected at the moment.

(defun eliminate-superfluous-temporaries (initial-instruction)
  ;; Make sure we are working with an up-to-date graph, both with
  ;; respect to instructions and with respect to data.
  (cleavir-ir:set-predecessors initial-instruction)
  (cleavir-ir:reinitialize-data initial-instruction)
  (let (;; This hash table contains the owner of every instruction.
	(owners (make-hash-table :test #'eq))
	;; This hash table contains ASSIGNMENT-INSTRUCTIONs that can
	;; be eliminated because they assign to a superfluous
	;; LEXICAL-LOCATION.
	(superfluous-instructions (make-hash-table :test #'eq))
	;; This hash table contains LEXICAL-LOCATIONs each of which is
	;; essential to keep because it is going to be used in place
	;; of some superfluous LEXICAL-LOCATION.
	(essential-locations (make-hash-table :test #'eq))
	;; This hash table contains LEXICAL-LOCATIONs each of which is
	;; superfluous because it is being assigned to from some
	;; essential LEXICAL-LOCATION.
	(superfluous-locations (make-hash-table :test #'eq)))
    ;; First compute the owner of each instruction
    (cleavir-ir:map-instructions-with-owner
     (lambda (instruction owner)
       (setf (gethash instruction owners) owner))
     initial-instruction)
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (when (and (typep instruction
			 'cleavir-ir:assignment-instruction)
		  (let ((input (car (cleavir-ir:inputs instruction)))
			(output (car (cleavir-ir:outputs instruction))))
		    (and (typep input 'cleavir-ir:lexical-location)
			 (= (length (cleavir-ir:defining-instructions input)) 1)
			 (eq (gethash instruction owners)
			     (gethash (car (cleavir-ir:defining-instructions input)) owners))
			 (not (gethash input superfluous-locations))
			 (typep output 'cleavir-ir:lexical-location)
			 (= (length (cleavir-ir:defining-instructions output)) 1)
			 (eq (gethash instruction owners)
			     (gethash (car (cleavir-ir:using-instructions output)) owners))
			 (not (gethash output essential-locations)))))
	 (setf (gethash instruction superfluous-instructions) t)
	 (setf (gethash (car (cleavir-ir:inputs instruction)) essential-locations) t)
	 (setf (gethash (car (cleavir-ir:outputs instruction)) superfluous-locations) t)))
     initial-instruction)
    (maphash (lambda (instruction value)
	       (declare (ignore value))
	       (let* ((input (car (cleavir-ir:inputs instruction)))
		      (output (car (cleavir-ir:outputs instruction)))
		      (using-instructions (cleavir-ir:using-instructions output)))
		 (loop for using-instruction in using-instructions
		       do (nsubstitute input output (cleavir-ir:inputs using-instruction)))
		 (setf (cleavir-ir:using-instructions input) using-instructions)
		 (cleavir-ir:delete-instruction instruction)))
	     superfluous-instructions)
    (cleavir-ir:set-predecessors initial-instruction)
    (cleavir-ir:reinitialize-data initial-instruction)
    ;; Return the number of instructions that were eliminated.
    (hash-table-count superfluous-instructions)))
