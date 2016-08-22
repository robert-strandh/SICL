(cl:in-package #:cleavir-type-inference)

(defun valid-bag-p (bag)
  (and (every (lambda (restriction)
		(and (consp restriction)
		     (typep (first restriction)
			    '(or cleavir-ir:lexical-location
			      cleavir-ir:values-location))
		     (symbolp (cdr restriction))))
	      bag)
       (= (length bag)
	  (length (remove-duplicates bag :test #'eq :key #'first)))))
