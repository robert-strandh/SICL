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

(defmethod compile-ast ((ast cleavir-ast:short-float-less-ast) context)
  (check-context-for-boolean-ast context)
  (let* ((arguments (cleavir-ast:children ast))
	 (temps (make-temps arguments)))
    (compile-and-unbox-arguments
     arguments
     temps
     'cleavir-mir:short-float-unbox-instruction
     (make-instance 'cleavir-mir:short-float-less-instruction
       :inputs temps
       :outputs '()
       :successors (successors context)))))
     
(defmethod compile-ast ((ast cleavir-ast:short-float-not-greater-ast) context)
  (check-context-for-boolean-ast context)
  (let* ((arguments (cleavir-ast:children ast))
	 (temps (make-temps arguments)))
    (compile-and-unbox-arguments
     arguments
     temps
     'cleavir-mir:short-float-unbox-instruction
     (make-instance 'cleavir-mir:short-float-not-greater-instruction
       :inputs temps
       :outputs '()
       :successors (successors context)))))

(defmethod compile-ast ((ast cleavir-ast:short-float-greater-ast) context)
  (check-context-for-boolean-ast context)
  (let* ((arguments (cleavir-ast:children ast))
	 (temps (make-temps arguments)))
    (compile-and-unbox-arguments
     arguments
     temps
     'cleavir-mir:short-float-unbox-instruction
     (make-instance 'cleavir-mir:short-float-less-instruction
       :inputs (reverse temps)
       :outputs '()
       :successors (successors context)))))
     
(defmethod compile-ast ((ast cleavir-ast:short-float-not-less-ast) context)
  (check-context-for-boolean-ast context)
  (let* ((arguments (cleavir-ast:children ast))
	 (temps (make-temps arguments)))
    (compile-and-unbox-arguments
     arguments
     temps
     'cleavir-mir:short-float-unbox-instruction
     (make-instance 'cleavir-mir:short-float-not-greater-instruction
       :inputs (reverse temps)
       :outputs '()
       :successors (successors context)))))
     
(defmethod compile-ast ((ast cleavir-ast:short-float-equal-ast) context)
  (check-context-for-boolean-ast context)
  (let* ((arguments (cleavir-ast:children ast))
	 (temps (make-temps arguments)))
    (compile-and-unbox-arguments
     arguments
     temps
     'cleavir-mir:short-float-unbox-instruction
     (make-instance 'cleavir-mir:short-float-equal-instruction
       :inputs (reverse temps)
       :outputs '()
       :successors (successors context)))))
