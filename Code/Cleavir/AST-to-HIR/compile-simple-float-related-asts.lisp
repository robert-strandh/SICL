(in-package #:cleavir-ast-to-hir)

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
;;; Method on COMPILE-AST for all simple floating-point arithmetic ASTs.

(defmacro compile-simple-float-arithmetic-ast
    (ast-class instruction-class)
  `(defmethod compile-ast ((ast ,ast-class) context)
     (let* ((arguments (cleavir-ast:children ast))
            (temps (make-temps arguments))
            (temp (cleavir-ir:new-temporary))
            (successor (make-instance 'cleavir-ir:box-instruction
                         :element-type (cleavir-ast:subtype ast)
                         :inputs (list temp)
                         :outputs (results context)
                         :successors (successors context))))
       (compile-and-unbox-arguments
        arguments
        temps
        (cleavir-ast:subtype ast)
        (make-instance ',instruction-class
          :subtype (cleavir-ast:subtype ast)
          :inputs temps
          :outputs (list temp)
          :successors (list successor))
        (invocation context)))))

(compile-simple-float-arithmetic-ast cleavir-ast:float-add-ast
                                     cleavir-ir:float-add-instruction)

(compile-simple-float-arithmetic-ast cleavir-ast:float-sub-ast
                                     cleavir-ir:float-sub-instruction)

(compile-simple-float-arithmetic-ast cleavir-ast:float-mul-ast
                                     cleavir-ir:float-mul-instruction)

(compile-simple-float-arithmetic-ast cleavir-ast:float-div-ast
                                     cleavir-ir:float-div-instruction)

(compile-simple-float-arithmetic-ast cleavir-ast:float-sin-ast
                                     cleavir-ir:float-sin-instruction)

(compile-simple-float-arithmetic-ast cleavir-ast:float-cos-ast
                                     cleavir-ir:float-cos-instruction)

(compile-simple-float-arithmetic-ast cleavir-ast:float-sqrt-ast
                                     cleavir-ir:float-sqrt-instruction)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on COMPILE-AST for all simple floating-point comparison ASTs. 

(defmacro compile-simple-float-comparison-ast
    (ast-class instruction-class input-transformer)
  `(defmethod compile-ast ((ast ,ast-class) context)
     (assert-context ast context 0 2)
     (let* ((subtype (cleavir-ast:subtype ast))
            (arguments (cleavir-ast:children ast))
            (temps (make-temps arguments)))
       (compile-and-unbox-arguments
        arguments
        temps
        subtype
        (make-instance ',instruction-class
          :subtype subtype
          :inputs (,input-transformer temps)
          :outputs '()
          :successors (successors context))
        (invocation context)))))

(compile-simple-float-comparison-ast cleavir-ast:float-less-ast
                                     cleavir-ir:float-less-instruction
                                     identity)

(compile-simple-float-comparison-ast cleavir-ast:float-not-greater-ast
                                     cleavir-ir:float-not-greater-instruction
                                     identity)

(compile-simple-float-comparison-ast cleavir-ast:float-equal-ast
                                     cleavir-ir:float-equal-instruction
                                     identity)

(compile-simple-float-comparison-ast cleavir-ast:float-not-less-ast
                                     cleavir-ir:float-not-greater-instruction
                                     reverse)

(compile-simple-float-comparison-ast cleavir-ast:float-greater-ast
                                     cleavir-ir:float-less-instruction
                                     reverse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a COERCE-AST.

(defmethod compile-ast ((ast cleavir-ast:coerce-ast) context)
  (let ((from (cleavir-ast:from-type ast)) (to (cleavir-ast:to-type ast))
        (arg (cleavir-ast:arg-ast ast))
        (input (cleavir-ir:new-temporary))
        (unboxed-input (cleavir-ir:new-temporary))
        (unboxed-output (cleavir-ir:new-temporary)))
    (compile-ast
     arg
     (context
      (list input)
      (list
       (make-instance 'cleavir-ir:unbox-instruction
         :element-type from
         :inputs (list input)
         :outputs (list unboxed-input)
         :successors (list (cleavir-ir:make-coerce-instruction
                             from to
                             unboxed-input unboxed-output
                             (make-instance 'cleavir-ir:box-instruction
                               :element-type to
                               :inputs (list unboxed-output)
                               :outputs (results context)
                               :successors (successors context))))))
      (invocation context)))))
