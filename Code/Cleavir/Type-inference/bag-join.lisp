(cl:in-package #:cleavir-type-inference)

(defun bag-join (bag &rest more-bags)
  (loop for b in (cons bag more-bags)
	do (assert (valid-bag-p b)))
  (loop for b in more-bags
	do (assert (bag-equal bag b)))
  (loop for (lexical . type-descriptor) in bag
	collect
	(cons lexical
	      (loop with result = type-descriptor
		    for b in more-bags
		    for r = (assoc lexical b :key #'first :test #'eq)
		    for type = (second r)
		    do (setf result (binary-join type-descriptor type))
		    finally (return result)))))
						  
		      
