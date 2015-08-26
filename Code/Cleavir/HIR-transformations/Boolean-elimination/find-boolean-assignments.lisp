(cl:in-package #:cleavir-boolean-elimination)

;;; We search for pairs of assignment instructions I1 and I2 such that
;;; I1 and I2 are the only predecessors of some instruction I, I1 and
;;; I2 assign to the same lexical location, and the inputs of I1 and
;;; I2 are both constant LOAD-TIME-VALUE-INPUTs with opposite Boolean
;;; values.
;;;
;;; We return a list of pairs (CONS cells) of assignment instructions
;;; that correspond to the criteria.
;;;
;;; At some point, we might want to improve this technique by not
;;; requiring I to be the immediate successor of I1 and I2, but
;;; instead allowing for non-branching paths from I1 to I and from I2
;;; to I.
(defun find-boolean-assignments (initial-instruction)
  (let ((result '()))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (let ((predecessors (cleavir-ir:predecessors instruction)))
	 (when (= (length predecessors) 2)
	   (destructuring-bind (p1 p2) predecessors
	     (when (and (typep p1 'cleavir-ir:assignment-instruction)
			(typep p2 'cleavir-ir:assignment-instruction))
	       (let ((i1 (first (cleavir-ir:inputs p1)))
		     (o1 (first (cleavir-ir:outputs p1)))
		     (i2 (first (cleavir-ir:inputs p2)))
		     (o2 (first (cleavir-ir:outputs p2))))
		 (when (and (eq o1 o2)
			    (or (and (boolean-input-p i1 nil)
				     (boolean-input-p i2 t))
				(and (boolean-input-p i1 t)
				     (boolean-input-p i2 nil))))
		   (push (cons p1 p2) result))))))))
     initial-instruction)
    result))
