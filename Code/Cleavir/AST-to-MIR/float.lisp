(cl:in-package #:cleavir-ast-to-mir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utility function for compiling a list of ASTs that are arguments
;;; to some operation, and that need to be unboxed.  They will all be
;;; compiled in a context with a single successor and a single result.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on COMPILE-AST for all floating-point arithmetic ASTs. 

(defmacro compile-float-arithmetic-ast
    (ast-class instruction-class unbox-instruction-class box-instruction-class)
  `(defmethod compile-ast ((ast ,ast-class) context)
     (check-context-for-one-value-ast context)
     (let* ((arguments (cleavir-ast:children ast))
	    (temps (make-temps arguments))
	    (temp (cleavir-mir:new-temporary))
	    (successor (make-instance ',box-instruction-class
			 :inputs (list temp)
			 :outputs (results context))))
       (compile-and-unbox-arguments
	arguments
	temps
	',unbox-instruction-class
	(make-instance ',instruction-class
	  :inputs temps
	  :outputs (car (results context))
	  :successors (list successor))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on COMPILE-AST for all floating-point comparison ASTs. 

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

(defmethod compile-ast ((ast cleavir-ast:single-float-less-ast) context)
  (check-context-for-boolean-ast context)
  (let* ((arguments (cleavir-ast:children ast))
	 (temps (make-temps arguments)))
    (compile-and-unbox-arguments
     arguments
     temps
     'cleavir-mir:single-float-unbox-instruction
     (make-instance 'cleavir-mir:single-float-less-instruction
       :inputs temps
       :outputs '()
       :successors (successors context)))))
     
(defmethod compile-ast ((ast cleavir-ast:single-float-not-greater-ast) context)
  (check-context-for-boolean-ast context)
  (let* ((arguments (cleavir-ast:children ast))
	 (temps (make-temps arguments)))
    (compile-and-unbox-arguments
     arguments
     temps
     'cleavir-mir:single-float-unbox-instruction
     (make-instance 'cleavir-mir:single-float-not-greater-instruction
       :inputs temps
       :outputs '()
       :successors (successors context)))))

(defmethod compile-ast ((ast cleavir-ast:single-float-greater-ast) context)
  (check-context-for-boolean-ast context)
  (let* ((arguments (cleavir-ast:children ast))
	 (temps (make-temps arguments)))
    (compile-and-unbox-arguments
     arguments
     temps
     'cleavir-mir:single-float-unbox-instruction
     (make-instance 'cleavir-mir:single-float-less-instruction
       :inputs (reverse temps)
       :outputs '()
       :successors (successors context)))))
     
(defmethod compile-ast ((ast cleavir-ast:single-float-not-less-ast) context)
  (check-context-for-boolean-ast context)
  (let* ((arguments (cleavir-ast:children ast))
	 (temps (make-temps arguments)))
    (compile-and-unbox-arguments
     arguments
     temps
     'cleavir-mir:single-float-unbox-instruction
     (make-instance 'cleavir-mir:single-float-not-greater-instruction
       :inputs (reverse temps)
       :outputs '()
       :successors (successors context)))))
     
(defmethod compile-ast ((ast cleavir-ast:single-float-equal-ast) context)
  (check-context-for-boolean-ast context)
  (let* ((arguments (cleavir-ast:children ast))
	 (temps (make-temps arguments)))
    (compile-and-unbox-arguments
     arguments
     temps
     'cleavir-mir:single-float-unbox-instruction
     (make-instance 'cleavir-mir:single-float-equal-instruction
       :inputs (reverse temps)
       :outputs '()
       :successors (successors context)))))

(defmethod compile-ast ((ast cleavir-ast:double-float-less-ast) context)
  (check-context-for-boolean-ast context)
  (let* ((arguments (cleavir-ast:children ast))
	 (temps (make-temps arguments)))
    (compile-and-unbox-arguments
     arguments
     temps
     'cleavir-mir:double-float-unbox-instruction
     (make-instance 'cleavir-mir:double-float-less-instruction
       :inputs temps
       :outputs '()
       :successors (successors context)))))
     
(defmethod compile-ast ((ast cleavir-ast:double-float-not-greater-ast) context)
  (check-context-for-boolean-ast context)
  (let* ((arguments (cleavir-ast:children ast))
	 (temps (make-temps arguments)))
    (compile-and-unbox-arguments
     arguments
     temps
     'cleavir-mir:double-float-unbox-instruction
     (make-instance 'cleavir-mir:double-float-not-greater-instruction
       :inputs temps
       :outputs '()
       :successors (successors context)))))

(defmethod compile-ast ((ast cleavir-ast:double-float-greater-ast) context)
  (check-context-for-boolean-ast context)
  (let* ((arguments (cleavir-ast:children ast))
	 (temps (make-temps arguments)))
    (compile-and-unbox-arguments
     arguments
     temps
     'cleavir-mir:double-float-unbox-instruction
     (make-instance 'cleavir-mir:double-float-less-instruction
       :inputs (reverse temps)
       :outputs '()
       :successors (successors context)))))
     
(defmethod compile-ast ((ast cleavir-ast:double-float-not-less-ast) context)
  (check-context-for-boolean-ast context)
  (let* ((arguments (cleavir-ast:children ast))
	 (temps (make-temps arguments)))
    (compile-and-unbox-arguments
     arguments
     temps
     'cleavir-mir:double-float-unbox-instruction
     (make-instance 'cleavir-mir:double-float-not-greater-instruction
       :inputs (reverse temps)
       :outputs '()
       :successors (successors context)))))
     
(defmethod compile-ast ((ast cleavir-ast:double-float-equal-ast) context)
  (check-context-for-boolean-ast context)
  (let* ((arguments (cleavir-ast:children ast))
	 (temps (make-temps arguments)))
    (compile-and-unbox-arguments
     arguments
     temps
     'cleavir-mir:double-float-unbox-instruction
     (make-instance 'cleavir-mir:double-float-equal-instruction
       :inputs (reverse temps)
       :outputs '()
       :successors (successors context)))))

(defmethod compile-ast ((ast cleavir-ast:long-float-less-ast) context)
  (check-context-for-boolean-ast context)
  (let* ((arguments (cleavir-ast:children ast))
	 (temps (make-temps arguments)))
    (compile-and-unbox-arguments
     arguments
     temps
     'cleavir-mir:long-float-unbox-instruction
     (make-instance 'cleavir-mir:long-float-less-instruction
       :inputs temps
       :outputs '()
       :successors (successors context)))))
     
(defmethod compile-ast ((ast cleavir-ast:long-float-not-greater-ast) context)
  (check-context-for-boolean-ast context)
  (let* ((arguments (cleavir-ast:children ast))
	 (temps (make-temps arguments)))
    (compile-and-unbox-arguments
     arguments
     temps
     'cleavir-mir:long-float-unbox-instruction
     (make-instance 'cleavir-mir:long-float-not-greater-instruction
       :inputs temps
       :outputs '()
       :successors (successors context)))))

(defmethod compile-ast ((ast cleavir-ast:long-float-greater-ast) context)
  (check-context-for-boolean-ast context)
  (let* ((arguments (cleavir-ast:children ast))
	 (temps (make-temps arguments)))
    (compile-and-unbox-arguments
     arguments
     temps
     'cleavir-mir:long-float-unbox-instruction
     (make-instance 'cleavir-mir:long-float-less-instruction
       :inputs (reverse temps)
       :outputs '()
       :successors (successors context)))))
     
(defmethod compile-ast ((ast cleavir-ast:long-float-not-less-ast) context)
  (check-context-for-boolean-ast context)
  (let* ((arguments (cleavir-ast:children ast))
	 (temps (make-temps arguments)))
    (compile-and-unbox-arguments
     arguments
     temps
     'cleavir-mir:long-float-unbox-instruction
     (make-instance 'cleavir-mir:long-float-not-greater-instruction
       :inputs (reverse temps)
       :outputs '()
       :successors (successors context)))))
     
(defmethod compile-ast ((ast cleavir-ast:long-float-equal-ast) context)
  (check-context-for-boolean-ast context)
  (let* ((arguments (cleavir-ast:children ast))
	 (temps (make-temps arguments)))
    (compile-and-unbox-arguments
     arguments
     temps
     'cleavir-mir:long-float-unbox-instruction
     (make-instance 'cleavir-mir:long-float-equal-instruction
       :inputs (reverse temps)
       :outputs '()
       :successors (successors context)))))
