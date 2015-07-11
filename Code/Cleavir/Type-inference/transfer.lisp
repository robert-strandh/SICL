(cl:in-package #:cleavir-type-inference)

(defgeneric transfer (instruction input-bag))

(defmethod transfer (instruction input-bag)
  (loop with result = input-bag
	for output in (cleavir-ir:outputs instruction)
	when (typep output 'cleavir-ir:lexical-location)
	  do (setf result
		   (cons (cons output t)
			 (remove output result :test #'eq :key #'first)))
	finally (return result)))
