(cl:in-package #:cleavir-ast-to-hir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utility function for compiling a list of ASTs that are arguments
;;; to some operation, and that need to be unboxed.  They will all be
;;; compiled in a context with a single successor and a single result.

(defun compile-and-unbox-arguments
    (arguments temps unbox-instruction-class successor invocation)
  (loop with succ = successor
	for arg in (reverse arguments)
	for temp in (reverse temps)
	for inter = (make-temp)
	do (setf succ
		 (make-instance unbox-instruction-class
		   :inputs (list inter)
		   :outputs (list temp)
		   :successors (list succ)))
	   (setf succ
		 (compile-ast arg (context `(,inter) `(,succ) invocation)))
	finally (return succ)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on COMPILE-AST for all floating-point arithmetic ASTs. 

(defmacro compile-float-arithmetic-ast
    (ast-class instruction-class unbox-instruction-class box-instruction-class)
  `(defmethod compile-ast ((ast ,ast-class) context)
     (let* ((arguments (cleavir-ast:children ast))
	    (temps (make-temps arguments))
	    (temp (cleavir-ir:new-temporary))
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
	  :successors (list successor))
	(invocation context)))))

(compile-float-arithmetic-ast cleavir-ast:short-float-add-ast
			      cleavir-ir:short-float-add-instruction
			      cleavir-ir:short-float-unbox-instruction
			      cleavir-ir:short-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:short-float-sub-ast
			      cleavir-ir:short-float-sub-instruction
			      cleavir-ir:short-float-unbox-instruction
			      cleavir-ir:short-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:short-float-mul-ast
			      cleavir-ir:short-float-mul-instruction
			      cleavir-ir:short-float-unbox-instruction
			      cleavir-ir:short-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:short-float-div-ast
			      cleavir-ir:short-float-div-instruction
			      cleavir-ir:short-float-unbox-instruction
			      cleavir-ir:short-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:short-float-sin-ast
			      cleavir-ir:short-float-sin-instruction
			      cleavir-ir:short-float-unbox-instruction
			      cleavir-ir:short-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:short-float-cos-ast
			      cleavir-ir:short-float-cos-instruction
			      cleavir-ir:short-float-unbox-instruction
			      cleavir-ir:short-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:short-float-sqrt-ast
			      cleavir-ir:short-float-sqrt-instruction
			      cleavir-ir:short-float-unbox-instruction
			      cleavir-ir:short-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:single-float-add-ast
			      cleavir-ir:single-float-add-instruction
			      cleavir-ir:single-float-unbox-instruction
			      cleavir-ir:single-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:single-float-sub-ast
			      cleavir-ir:single-float-sub-instruction
			      cleavir-ir:single-float-unbox-instruction
			      cleavir-ir:single-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:single-float-mul-ast
			      cleavir-ir:single-float-mul-instruction
			      cleavir-ir:single-float-unbox-instruction
			      cleavir-ir:single-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:single-float-div-ast
			      cleavir-ir:single-float-div-instruction
			      cleavir-ir:single-float-unbox-instruction
			      cleavir-ir:single-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:single-float-sin-ast
			      cleavir-ir:single-float-sin-instruction
			      cleavir-ir:single-float-unbox-instruction
			      cleavir-ir:single-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:single-float-cos-ast
			      cleavir-ir:single-float-cos-instruction
			      cleavir-ir:single-float-unbox-instruction
			      cleavir-ir:single-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:single-float-sqrt-ast
			      cleavir-ir:single-float-sqrt-instruction
			      cleavir-ir:single-float-unbox-instruction
			      cleavir-ir:single-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:double-float-add-ast
			      cleavir-ir:double-float-add-instruction
			      cleavir-ir:double-float-unbox-instruction
			      cleavir-ir:double-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:double-float-sub-ast
			      cleavir-ir:double-float-sub-instruction
			      cleavir-ir:double-float-unbox-instruction
			      cleavir-ir:double-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:double-float-mul-ast
			      cleavir-ir:double-float-mul-instruction
			      cleavir-ir:double-float-unbox-instruction
			      cleavir-ir:double-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:double-float-div-ast
			      cleavir-ir:double-float-div-instruction
			      cleavir-ir:double-float-unbox-instruction
			      cleavir-ir:double-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:double-float-sin-ast
			      cleavir-ir:double-float-sin-instruction
			      cleavir-ir:double-float-unbox-instruction
			      cleavir-ir:double-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:double-float-cos-ast
			      cleavir-ir:double-float-cos-instruction
			      cleavir-ir:double-float-unbox-instruction
			      cleavir-ir:double-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:double-float-sqrt-ast
			      cleavir-ir:double-float-sqrt-instruction
			      cleavir-ir:double-float-unbox-instruction
			      cleavir-ir:double-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:long-float-add-ast
			      cleavir-ir:long-float-add-instruction
			      cleavir-ir:long-float-unbox-instruction
			      cleavir-ir:long-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:long-float-sub-ast
			      cleavir-ir:long-float-sub-instruction
			      cleavir-ir:long-float-unbox-instruction
			      cleavir-ir:long-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:long-float-mul-ast
			      cleavir-ir:long-float-mul-instruction
			      cleavir-ir:long-float-unbox-instruction
			      cleavir-ir:long-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:long-float-div-ast
			      cleavir-ir:long-float-div-instruction
			      cleavir-ir:long-float-unbox-instruction
			      cleavir-ir:long-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:long-float-sin-ast
			      cleavir-ir:long-float-sin-instruction
			      cleavir-ir:long-float-unbox-instruction
			      cleavir-ir:long-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:long-float-cos-ast
			      cleavir-ir:long-float-cos-instruction
			      cleavir-ir:long-float-unbox-instruction
			      cleavir-ir:long-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:long-float-sqrt-ast
			      cleavir-ir:long-float-sqrt-instruction
			      cleavir-ir:long-float-unbox-instruction
			      cleavir-ir:long-float-box-instruction)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on COMPILE-AST for all floating-point comparison ASTs. 

(defmacro compile-float-comparison-ast
    (ast-class instruction-class unbox-instruction-class input-transformer)
  `(defmethod compile-ast ((ast ,ast-class) context)
     (assert-context ast context 0 2)
     (check-context-for-boolean-ast context)
     (let* ((arguments (cleavir-ast:children ast))
	    (temps (make-temps arguments)))
       (compile-and-unbox-arguments
	arguments
	temps
	',unbox-instruction-class
	(make-instance ',instruction-class
	  :inputs (,input-transformer temps)
	  :outputs '()
	  :successors (successors context))
	(invocation context)))))

(compile-float-comparison-ast cleavir-ast:short-float-less-ast
			      cleavir-ir:short-float-less-instruction
			      cleavir-ir:short-float-unbox-instruction
			      identity)
     
(compile-float-comparison-ast cleavir-ast:short-float-not-greater-ast
			      cleavir-ir:short-float-not-greater-instruction
			      cleavir-ir:short-float-unbox-instruction
			      identity)

(compile-float-comparison-ast cleavir-ast:short-float-greater-ast
			      cleavir-ir:short-float-less-instruction
			      cleavir-ir:short-float-unbox-instruction
			      reverse)
     
(compile-float-comparison-ast cleavir-ast:short-float-not-less-ast
			      cleavir-ir:short-float-not-greater-instruction
			      cleavir-ir:short-float-unbox-instruction
			      reverse)

(compile-float-comparison-ast cleavir-ast:short-float-equal-ast
			      cleavir-ir:short-float-equal-instruction
			      cleavir-ir:short-float-unbox-instruction
			      identity)

(compile-float-comparison-ast cleavir-ast:single-float-less-ast
			      cleavir-ir:single-float-less-instruction
			      cleavir-ir:single-float-unbox-instruction
			      identity)
     
(compile-float-comparison-ast cleavir-ast:single-float-not-greater-ast
			      cleavir-ir:single-float-not-greater-instruction
			      cleavir-ir:single-float-unbox-instruction
			      identity)

(compile-float-comparison-ast cleavir-ast:single-float-greater-ast
			      cleavir-ir:single-float-less-instruction
			      cleavir-ir:single-float-unbox-instruction
			      reverse)
     
(compile-float-comparison-ast cleavir-ast:single-float-not-less-ast
			      cleavir-ir:single-float-not-greater-instruction
			      cleavir-ir:single-float-unbox-instruction
			      reverse)

(compile-float-comparison-ast cleavir-ast:single-float-equal-ast
			      cleavir-ir:single-float-equal-instruction
			      cleavir-ir:single-float-unbox-instruction
			      identity)

(compile-float-comparison-ast cleavir-ast:double-float-less-ast
			      cleavir-ir:double-float-less-instruction
			      cleavir-ir:double-float-unbox-instruction
			      identity)
     
(compile-float-comparison-ast cleavir-ast:double-float-not-greater-ast
			      cleavir-ir:double-float-not-greater-instruction
			      cleavir-ir:double-float-unbox-instruction
			      identity)

(compile-float-comparison-ast cleavir-ast:double-float-greater-ast
			      cleavir-ir:double-float-less-instruction
			      cleavir-ir:double-float-unbox-instruction
			      reverse)
     
(compile-float-comparison-ast cleavir-ast:double-float-not-less-ast
			      cleavir-ir:double-float-not-greater-instruction
			      cleavir-ir:double-float-unbox-instruction
			      reverse)

(compile-float-comparison-ast cleavir-ast:double-float-equal-ast
			      cleavir-ir:double-float-equal-instruction
			      cleavir-ir:double-float-unbox-instruction
			      identity)

(compile-float-comparison-ast cleavir-ast:long-float-less-ast
			      cleavir-ir:long-float-less-instruction
			      cleavir-ir:long-float-unbox-instruction
			      identity)
     
(compile-float-comparison-ast cleavir-ast:long-float-not-greater-ast
			      cleavir-ir:long-float-not-greater-instruction
			      cleavir-ir:long-float-unbox-instruction
			      identity)

(compile-float-comparison-ast cleavir-ast:long-float-greater-ast
			      cleavir-ir:long-float-less-instruction
			      cleavir-ir:long-float-unbox-instruction
			      reverse)
     
(compile-float-comparison-ast cleavir-ast:long-float-not-less-ast
			      cleavir-ir:long-float-not-greater-instruction
			      cleavir-ir:long-float-unbox-instruction
			      reverse)

(compile-float-comparison-ast cleavir-ast:long-float-equal-ast
			      cleavir-ir:long-float-equal-instruction
			      cleavir-ir:long-float-unbox-instruction
			      identity)

