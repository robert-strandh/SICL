(cl:in-package #:cleavir-ast-to-hir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utility function for compiling a list of ASTs that are arguments
;;; to some operation, and that need to be unboxed.  They will all be
;;; compiled in a context with a single successor and a single result.

(defun compile-and-unbox-arguments
    (arguments temps element-type successor invocation)
  (loop with succ = successor
	for arg in (reverse arguments)
	for temp in (reverse temps)
	for inter = (make-temp)
	do (setf succ
		 (make-instance 'cleavir-ir:unbox-instruction
		   :element-type element-type
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
    (ast-class instruction-class element-type)
  `(defmethod compile-ast ((ast ,ast-class) context)
     (let* ((arguments (cleavir-ast:children ast))
	    (temps (make-temps arguments))
	    (temp (cleavir-ir:new-temporary))
	    (successor (make-instance 'cleavir-ir:box-instruction
			 :element-type ',element-type
			 :inputs (list temp)
			 :outputs (results context)
			 :successors (successors context))))
       (compile-and-unbox-arguments
	arguments
	temps
	',element-type
	(make-instance ',instruction-class
	  :inputs temps
	  :outputs (results context)
	  :successors (list successor))
	(invocation context)))))

(compile-float-arithmetic-ast cleavir-ast:short-float-add-ast
			      cleavir-ir:short-float-add-instruction
			      short-float)

(compile-float-arithmetic-ast cleavir-ast:short-float-sub-ast
			      cleavir-ir:short-float-sub-instruction
			      short-float)

(compile-float-arithmetic-ast cleavir-ast:short-float-mul-ast
			      cleavir-ir:short-float-mul-instruction
			      short-float)

(compile-float-arithmetic-ast cleavir-ast:short-float-div-ast
			      cleavir-ir:short-float-div-instruction
			      short-float)

(compile-float-arithmetic-ast cleavir-ast:short-float-sin-ast
			      cleavir-ir:short-float-sin-instruction
			      short-float)

(compile-float-arithmetic-ast cleavir-ast:short-float-cos-ast
			      cleavir-ir:short-float-cos-instruction
			      short-float)

(compile-float-arithmetic-ast cleavir-ast:short-float-sqrt-ast
			      cleavir-ir:short-float-sqrt-instruction
			      short-float)

(compile-float-arithmetic-ast cleavir-ast:single-float-add-ast
			      cleavir-ir:single-float-add-instruction
			      short-float)

(compile-float-arithmetic-ast cleavir-ast:single-float-sub-ast
			      cleavir-ir:single-float-sub-instruction
			      single-float)

(compile-float-arithmetic-ast cleavir-ast:single-float-mul-ast
			      cleavir-ir:single-float-mul-instruction
			      single-float)

(compile-float-arithmetic-ast cleavir-ast:single-float-div-ast
			      cleavir-ir:single-float-div-instruction
			      single-float)

(compile-float-arithmetic-ast cleavir-ast:single-float-sin-ast
			      cleavir-ir:single-float-sin-instruction
			      single-float)

(compile-float-arithmetic-ast cleavir-ast:single-float-cos-ast
			      cleavir-ir:single-float-cos-instruction
			      single-float)

(compile-float-arithmetic-ast cleavir-ast:single-float-sqrt-ast
			      cleavir-ir:single-float-sqrt-instruction
			      single-float)

(compile-float-arithmetic-ast cleavir-ast:double-float-add-ast
			      cleavir-ir:double-float-add-instruction
			      double-float)

(compile-float-arithmetic-ast cleavir-ast:double-float-sub-ast
			      cleavir-ir:double-float-sub-instruction
			      double-float)

(compile-float-arithmetic-ast cleavir-ast:double-float-mul-ast
			      cleavir-ir:double-float-mul-instruction
			      double-float)

(compile-float-arithmetic-ast cleavir-ast:double-float-div-ast
			      cleavir-ir:double-float-div-instruction
			      double-float)

(compile-float-arithmetic-ast cleavir-ast:double-float-sin-ast
			      cleavir-ir:double-float-sin-instruction
			      double-float)

(compile-float-arithmetic-ast cleavir-ast:double-float-cos-ast
			      cleavir-ir:double-float-cos-instruction
			      double-float)

(compile-float-arithmetic-ast cleavir-ast:double-float-sqrt-ast
			      cleavir-ir:double-float-sqrt-instruction
			      double-float)

(compile-float-arithmetic-ast cleavir-ast:long-float-add-ast
			      cleavir-ir:long-float-add-instruction
			      long-float)

(compile-float-arithmetic-ast cleavir-ast:long-float-sub-ast
			      cleavir-ir:long-float-sub-instruction
			      long-float)

(compile-float-arithmetic-ast cleavir-ast:long-float-mul-ast
			      cleavir-ir:long-float-mul-instruction
			      long-float)

(compile-float-arithmetic-ast cleavir-ast:long-float-div-ast
			      cleavir-ir:long-float-div-instruction
			      long-float)

(compile-float-arithmetic-ast cleavir-ast:long-float-sin-ast
			      cleavir-ir:long-float-sin-instruction
			      long-float)

(compile-float-arithmetic-ast cleavir-ast:long-float-cos-ast
			      cleavir-ir:long-float-cos-instruction
			      long-float)

(compile-float-arithmetic-ast cleavir-ast:long-float-sqrt-ast
			      cleavir-ir:long-float-sqrt-instruction
			      long-float)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on COMPILE-AST for all floating-point comparison ASTs. 

(defmacro compile-float-comparison-ast
    (ast-class instruction-class element-type input-transformer)
  `(defmethod compile-ast ((ast ,ast-class) context)
     (assert-context ast context 0 2)
     (let* ((arguments (cleavir-ast:children ast))
	    (temps (make-temps arguments)))
       (compile-and-unbox-arguments
	arguments
	temps
	',element-type
	(make-instance ',instruction-class
	  :inputs (,input-transformer temps)
	  :outputs '()
	  :successors (successors context))
	(invocation context)))))

(compile-float-comparison-ast cleavir-ast:short-float-less-ast
			      cleavir-ir:short-float-less-instruction
			      short-float
			      identity)
     
(compile-float-comparison-ast cleavir-ast:short-float-not-greater-ast
			      cleavir-ir:short-float-not-greater-instruction
			      short-float
			      identity)

(compile-float-comparison-ast cleavir-ast:short-float-greater-ast
			      cleavir-ir:short-float-less-instruction
			      short-float
			      reverse)
     
(compile-float-comparison-ast cleavir-ast:short-float-not-less-ast
			      cleavir-ir:short-float-not-greater-instruction
			      short-float
			      reverse)

(compile-float-comparison-ast cleavir-ast:short-float-equal-ast
			      cleavir-ir:short-float-equal-instruction
			      short-float
			      identity)

(compile-float-comparison-ast cleavir-ast:single-float-less-ast
			      cleavir-ir:single-float-less-instruction
			      single-float
			      identity)
     
(compile-float-comparison-ast cleavir-ast:single-float-not-greater-ast
			      cleavir-ir:single-float-not-greater-instruction
			      single-float
			      identity)

(compile-float-comparison-ast cleavir-ast:single-float-greater-ast
			      cleavir-ir:single-float-less-instruction
			      single-float
			      reverse)
     
(compile-float-comparison-ast cleavir-ast:single-float-not-less-ast
			      cleavir-ir:single-float-not-greater-instruction
			      single-float
			      reverse)

(compile-float-comparison-ast cleavir-ast:single-float-equal-ast
			      cleavir-ir:single-float-equal-instruction
			      single-float
			      identity)

(compile-float-comparison-ast cleavir-ast:double-float-less-ast
			      cleavir-ir:double-float-less-instruction
			      double-float
			      identity)
     
(compile-float-comparison-ast cleavir-ast:double-float-not-greater-ast
			      cleavir-ir:double-float-not-greater-instruction
			      double-float
			      identity)

(compile-float-comparison-ast cleavir-ast:double-float-greater-ast
			      cleavir-ir:double-float-less-instruction
			      double-float
			      reverse)
     
(compile-float-comparison-ast cleavir-ast:double-float-not-less-ast
			      cleavir-ir:double-float-not-greater-instruction
			      double-float
			      reverse)

(compile-float-comparison-ast cleavir-ast:double-float-equal-ast
			      cleavir-ir:double-float-equal-instruction
			      double-float
			      identity)

(compile-float-comparison-ast cleavir-ast:long-float-less-ast
			      cleavir-ir:long-float-less-instruction
			      long-float
			      identity)
     
(compile-float-comparison-ast cleavir-ast:long-float-not-greater-ast
			      cleavir-ir:long-float-not-greater-instruction
			      long-float
			      identity)

(compile-float-comparison-ast cleavir-ast:long-float-greater-ast
			      cleavir-ir:long-float-less-instruction
			      long-float
			      reverse)
     
(compile-float-comparison-ast cleavir-ast:long-float-not-less-ast
			      cleavir-ir:long-float-not-greater-instruction
			      long-float
			      reverse)

(compile-float-comparison-ast cleavir-ast:long-float-equal-ast
			      cleavir-ir:long-float-equal-instruction
			      long-float
			      identity)

