(cl:in-package #:cleavir-ast-to-mir)

(defun compile-and-unbox-arguments
    (arguments temps unbox-instruction-class successor)
  (loop with succ = successor
	for arg in (reverse arguments)
	for temp in (reverse temps)
	for inter = (make-temp nil)
	do (setf succ
		 (make-instance unbox-instruction-class
		   :inputs (list inter)
		   :outputs (list temp)
		   :successors (list succ)))
	   (setf succ (compile-ast arg (context `(,inter) `(,succ))))
	finally (return succ)))

(defun check-context-for-boolean-ast (context)
  (assert (and (zerop (length (results context)))
	       (= (length (successors context)) 2))))
