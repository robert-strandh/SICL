(cl:in-package #:cleavir-hir-transformations)

(defun hoist-load-time-values (initial-instruction)
  (assert (typep initial-instruction 'cleavir-ir:top-level-enter-instruction))
  (let ((load-time-value-inputs '()))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (loop for input in (cleavir-ir:inputs instruction)
	     when (typep input 'cleavir-ir:load-time-value-input)
	       do (push input load-time-value-inputs)))
     initial-instruction)
    (let ((forms (mapcar #'cleavir-ir:form load-time-value-inputs)))
      (loop for input in load-time-value-inputs
	    do (change-class input
			     'cleavir-ir:lexical-location
			     :name (gensym)))
      (setf (cleavir-ir:forms initial-instruction)
	    (append (cleavir-ir:forms initial-instruction) forms))
      (setf (cleavir-ir:outputs initial-instruction)
	    (append (cleavir-ir:outputs initial-instruction)
		    load-time-value-inputs)))))
