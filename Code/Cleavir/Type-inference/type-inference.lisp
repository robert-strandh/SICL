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
	(result (make-dictionary)))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (loop for predecessor in (remove-duplicates
				 (cleavir-ir:predecessors instruction))
	     for live = (cleavir-liveness:live-before liveness instruction)
	     do (loop for var in live
		      when (typep var 'cleavir-ir:lexical-location)
			do (push (cons var t)
				 (arc-bag predecessor instruction
					  result))
		      when (typep var 'cleavir-ir:values-location)
			do (push (cons
				  var
				  ;; top type
				  (approximate-values nil nil t))
				 (arc-bag predecessor instruction
					  result)))))
     initial-instruction)
    result))

(defun infer-types (initial-instruction)
  (let ((*work-list* (compute-initial-work-list initial-instruction))
	(*dictionary* (compute-initial-dictionary initial-instruction)))
    (loop until (null *work-list*)
	  do (process-instruction (pop *work-list*)))
    *dictionary*))
