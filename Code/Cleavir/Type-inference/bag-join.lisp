(cl:in-package #:cleavir-type-inference)

(defun bag-join (&rest bags)
  (when (null bags)
    (return-from bag-join nil)) ; an empty bag.
  (loop for b in bags
	do (assert (valid-bag-p b)))
  (loop for (lexical . type-descriptor) in (first bags)
	collect
	(cons lexical
	      (loop with result = type-descriptor
		    for b in (rest bags)
		    for r = (assoc lexical b :test #'eq)
		    for type = (cdr r)
		    do (setf result (binary-join type-descriptor type))
		    finally (return result)))))
