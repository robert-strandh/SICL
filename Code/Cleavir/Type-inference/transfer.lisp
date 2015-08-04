(cl:in-package #:cleavir-type-inference)

(defgeneric one-successor-transfer (instruction input-bag))

(defgeneric two-successors-treansfer (instruction input-bag))

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
