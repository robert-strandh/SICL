(cl:in-package #:cleavir-ast-to-hir)

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
	    (temp (cleavir-hir:new-temporary))
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

(compile-float-arithmetic-ast cleavir-ast:short-float-add-ast
			      cleavir-hir:short-float-add-instruction
			      cleavir-hir:short-float-unbox-instruction
			      cleavir-hir:short-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:short-float-sub-ast
			      cleavir-hir:short-float-sub-instruction
			      cleavir-hir:short-float-unbox-instruction
			      cleavir-hir:short-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:short-float-mul-ast
			      cleavir-hir:short-float-mul-instruction
			      cleavir-hir:short-float-unbox-instruction
			      cleavir-hir:short-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:short-float-div-ast
			      cleavir-hir:short-float-div-instruction
			      cleavir-hir:short-float-unbox-instruction
			      cleavir-hir:short-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:short-float-sin-ast
			      cleavir-hir:short-float-sin-instruction
			      cleavir-hir:short-float-unbox-instruction
			      cleavir-hir:short-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:short-float-cos-ast
			      cleavir-hir:short-float-cos-instruction
			      cleavir-hir:short-float-unbox-instruction
			      cleavir-hir:short-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:short-float-sqrt-ast
			      cleavir-hir:short-float-sqrt-instruction
			      cleavir-hir:short-float-unbox-instruction
			      cleavir-hir:short-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:single-float-add-ast
			      cleavir-hir:single-float-add-instruction
			      cleavir-hir:single-float-unbox-instruction
			      cleavir-hir:single-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:single-float-sub-ast
			      cleavir-hir:single-float-sub-instruction
			      cleavir-hir:single-float-unbox-instruction
			      cleavir-hir:single-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:single-float-mul-ast
			      cleavir-hir:single-float-mul-instruction
			      cleavir-hir:single-float-unbox-instruction
			      cleavir-hir:single-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:single-float-div-ast
			      cleavir-hir:single-float-div-instruction
			      cleavir-hir:single-float-unbox-instruction
			      cleavir-hir:single-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:single-float-sin-ast
			      cleavir-hir:single-float-sin-instruction
			      cleavir-hir:single-float-unbox-instruction
			      cleavir-hir:single-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:single-float-cos-ast
			      cleavir-hir:single-float-cos-instruction
			      cleavir-hir:single-float-unbox-instruction
			      cleavir-hir:single-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:single-float-sqrt-ast
			      cleavir-hir:single-float-sqrt-instruction
			      cleavir-hir:single-float-unbox-instruction
			      cleavir-hir:single-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:double-float-add-ast
			      cleavir-hir:double-float-add-instruction
			      cleavir-hir:double-float-unbox-instruction
			      cleavir-hir:double-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:double-float-sub-ast
			      cleavir-hir:double-float-sub-instruction
			      cleavir-hir:double-float-unbox-instruction
			      cleavir-hir:double-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:double-float-mul-ast
			      cleavir-hir:double-float-mul-instruction
			      cleavir-hir:double-float-unbox-instruction
			      cleavir-hir:double-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:double-float-div-ast
			      cleavir-hir:double-float-div-instruction
			      cleavir-hir:double-float-unbox-instruction
			      cleavir-hir:double-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:double-float-sin-ast
			      cleavir-hir:double-float-sin-instruction
			      cleavir-hir:double-float-unbox-instruction
			      cleavir-hir:double-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:double-float-cos-ast
			      cleavir-hir:double-float-cos-instruction
			      cleavir-hir:double-float-unbox-instruction
			      cleavir-hir:double-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:double-float-sqrt-ast
			      cleavir-hir:double-float-sqrt-instruction
			      cleavir-hir:double-float-unbox-instruction
			      cleavir-hir:double-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:long-float-add-ast
			      cleavir-hir:long-float-add-instruction
			      cleavir-hir:long-float-unbox-instruction
			      cleavir-hir:long-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:long-float-sub-ast
			      cleavir-hir:long-float-sub-instruction
			      cleavir-hir:long-float-unbox-instruction
			      cleavir-hir:long-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:long-float-mul-ast
			      cleavir-hir:long-float-mul-instruction
			      cleavir-hir:long-float-unbox-instruction
			      cleavir-hir:long-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:long-float-div-ast
			      cleavir-hir:long-float-div-instruction
			      cleavir-hir:long-float-unbox-instruction
			      cleavir-hir:long-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:long-float-sin-ast
			      cleavir-hir:long-float-sin-instruction
			      cleavir-hir:long-float-unbox-instruction
			      cleavir-hir:long-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:long-float-cos-ast
			      cleavir-hir:long-float-cos-instruction
			      cleavir-hir:long-float-unbox-instruction
			      cleavir-hir:long-float-box-instruction)

(compile-float-arithmetic-ast cleavir-ast:long-float-sqrt-ast
			      cleavir-hir:long-float-sqrt-instruction
			      cleavir-hir:long-float-unbox-instruction
			      cleavir-hir:long-float-box-instruction)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on COMPILE-AST for all floating-point comparison ASTs. 

(defmacro compile-float-comparison-ast
    (ast-class instruction-class unbox-instruction-class input-transformer)
  `(defmethod compile-ast ((ast ,ast-class) context)
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
	  :successors (successors context))))))

(compile-float-comparison-ast cleavir-ast:short-float-less-ast
			      cleavir-hir:short-float-less-instruction
			      cleavir-hir:short-float-unbox-instruction
			      identity)
     
(compile-float-comparison-ast cleavir-ast:short-float-not-greater-ast
			      cleavir-hir:short-float-not-greater-instruction
			      cleavir-hir:short-float-unbox-instruction
			      identity)

(compile-float-comparison-ast cleavir-ast:short-float-greater-ast
			      cleavir-hir:short-float-less-instruction
			      cleavir-hir:short-float-unbox-instruction
			      reverse)
     
(compile-float-comparison-ast cleavir-ast:short-float-not-less-ast
			      cleavir-hir:short-float-not-greater-instruction
			      cleavir-hir:short-float-unbox-instruction
			      reverse)

(compile-float-comparison-ast cleavir-ast:short-float-equal-ast
			      cleavir-hir:short-float-equal-instruction
			      cleavir-hir:short-float-unbox-instruction
			      identity)

(compile-float-comparison-ast cleavir-ast:single-float-less-ast
			      cleavir-hir:single-float-less-instruction
			      cleavir-hir:single-float-unbox-instruction
			      identity)
     
(compile-float-comparison-ast cleavir-ast:single-float-not-greater-ast
			      cleavir-hir:single-float-not-greater-instruction
			      cleavir-hir:single-float-unbox-instruction
			      identity)

(compile-float-comparison-ast cleavir-ast:single-float-greater-ast
			      cleavir-hir:single-float-less-instruction
			      cleavir-hir:single-float-unbox-instruction
			      reverse)
     
(compile-float-comparison-ast cleavir-ast:single-float-not-less-ast
			      cleavir-hir:single-float-not-greater-instruction
			      cleavir-hir:single-float-unbox-instruction
			      reverse)

(compile-float-comparison-ast cleavir-ast:single-float-equal-ast
			      cleavir-hir:single-float-equal-instruction
			      cleavir-hir:single-float-unbox-instruction
			      identity)

(compile-float-comparison-ast cleavir-ast:double-float-less-ast
			      cleavir-hir:double-float-less-instruction
			      cleavir-hir:double-float-unbox-instruction
			      identity)
     
(compile-float-comparison-ast cleavir-ast:double-float-not-greater-ast
			      cleavir-hir:double-float-not-greater-instruction
			      cleavir-hir:double-float-unbox-instruction
			      identity)

(compile-float-comparison-ast cleavir-ast:double-float-greater-ast
			      cleavir-hir:double-float-less-instruction
			      cleavir-hir:double-float-unbox-instruction
			      reverse)
     
(compile-float-comparison-ast cleavir-ast:double-float-not-less-ast
			      cleavir-hir:double-float-not-greater-instruction
			      cleavir-hir:double-float-unbox-instruction
			      reverse)

(compile-float-comparison-ast cleavir-ast:double-float-equal-ast
			      cleavir-hir:double-float-equal-instruction
			      cleavir-hir:double-float-unbox-instruction
			      identity)

(compile-float-comparison-ast cleavir-ast:long-float-less-ast
			      cleavir-hir:long-float-less-instruction
			      cleavir-hir:long-float-unbox-instruction
			      identity)
     
(compile-float-comparison-ast cleavir-ast:long-float-not-greater-ast
			      cleavir-hir:long-float-not-greater-instruction
			      cleavir-hir:long-float-unbox-instruction
			      identity)

(compile-float-comparison-ast cleavir-ast:long-float-greater-ast
			      cleavir-hir:long-float-less-instruction
			      cleavir-hir:long-float-unbox-instruction
			      reverse)
     
(compile-float-comparison-ast cleavir-ast:long-float-not-less-ast
			      cleavir-hir:long-float-not-greater-instruction
			      cleavir-hir:long-float-unbox-instruction
			      reverse)

(compile-float-comparison-ast cleavir-ast:long-float-equal-ast
			      cleavir-hir:long-float-equal-instruction
			      cleavir-hir:long-float-unbox-instruction
			      identity)

