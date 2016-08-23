(cl:in-package #:cleavir-type-inference)

(defun compute-initial-work-list (initial-instruction)
  (let ((result '()))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (unless (typep instruction 'cleavir-ir:enter-instruction)
	 (push instruction result)))
     initial-instruction)
    result))

(defun compute-initial-dictionary (initial-instruction)
  (let ((liveness (cleavir-liveness:liveness initial-instruction))
	(result (make-hash-table :test #'equal)))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (loop for predecessor in (cleavir-ir:predecessors instruction)
	     for key = (cons predecessor instruction)
	     for live = (cleavir-liveness:live-before liveness instruction)
	     do (loop for var in live
		      when (typep var 'cleavir-ir:lexical-location)
		      do (push (cons var t)
			       (gethash (cons predecessor instruction)
					result)))))
     initial-instruction)
    result))

(defun process-instruction (instruction)
  (loop with successor-count = (length (cleavir-ir:successors instruction))
	for predecessor in (cleavir-ir:predecessors instruction)
	for key = (cons predecessor instruction)
	for bag-input = (gethash key *dictionary*)
	do (ecase successor-count
	     (0 nil)
	     (1 (one-successor-transfer instruction bag-input))
	     (2 (two-successors-transfer instruction bag-input)))))

(defun infer-types (initial-instruction)
  (let ((*work-list* (compute-initial-work-list initial-instruction))
	(*dictionary* (compute-initial-dictionary initial-instruction)))
    (loop until (null *work-list*)
	  do (process-instruction (pop *work-list*)))
    *dictionary*))
