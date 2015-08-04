(cl:in-package #:cleavir-type-inference)

(defgeneric one-successor-transfer (instruction input-bag))

(defgeneric two-successors-treansfer (instruction input-bag))

(defmethod one-successor-transfer :around (instruction input-bag)
  (let* ((result (call-next-method))
	 (successor (first (cleavir-ir:successors instruction)))
	 (key (cons instruction successor))
	 (existing (gethash key *dictionary*))
	 (filtered-result (filter result existing)))
    (unless (bag-equal filtered-result existing)
      (push successor *work-list*)
      (setf (gethash key *dictionary*) filtered-result))))

(defmethod one-successor-transfer (instruction input-bag)
  (loop with result = input-bag
	for output in (cleavir-ir:outputs instruction)
	when (typep output 'cleavir-ir:lexical-location)
	  do (setf result (update output t result))
	finally (return result)))

(defmethod two-successors-treansfer (instruction input-bag)
  (loop with result = input-bag
	for output in (cleavir-ir:outputs instruction)
	when (typep output 'cleavir-ir:lexical-location)
	  do (setf result (update output t result))
	finally (return (values result result))))
