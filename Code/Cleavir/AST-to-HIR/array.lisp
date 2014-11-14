(cl:in-package #:cleavir-ast-to-hir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a T-AREF-AST

(defmethod compile-ast ((ast cleavir-ast:t-aref-ast) context)
  (check-context-for-one-value-ast context)
  (let ((temp1 (make-temp nil))
	(temp2 (make-temp nil)))
    (compile-ast
     (cleavir-ast:array-ast ast)
     (context
      (list temp1)
      (list (compile-ast
	     (cleavir-ast:index-ast ast)
	     (context (list temp2)
		      (list (make-instance 'cleavir-ir:t-aref-instruction
			      :inputs (list temp1 temp2)
			      :outputs (results context)
			      :successors (successors context)))
		      (invocation context))))
      (invocation context)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a T-ASET-AST

(defmethod compile-ast ((ast cleavir-ast:t-aset-ast) context)
  (check-context-for-no-value-ast context)
  (let ((temp1 (make-temp nil))
	(temp2 (make-temp nil))
	(temp3 (make-temp nil)))
    (compile-ast
     (cleavir-ast:array-ast ast)
     (context
      (list temp1)
      (list (compile-ast
	     (cleavir-ast:index-ast ast)
	     (context
	      (list temp2)
	      (compile-ast
	       (cleavir-ast:value-ast ast)
	       (context
		(list temp3)
		(list (make-instance 'cleavir-ir:t-aset-instruction
			:inputs (list temp1 temp2 temp3)
			:outputs '()
			:successors (successors context)))
		(invocation context)))
	      (invocation context))))
      (invocation context)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro for compiling specialized AREF ASTs

(defmacro compile-specialized-aref-ast
    (ast-class aref-instruction-class box-instruction-class)
  `(defmethod compile-ast ((ast ,ast-class) context)
     (check-context-for-one-value-ast context)
     (let ((temp1 (make-temp nil))
	   (temp2 (make-temp nil))
	   (temp3 (make-temp nil)))
       (compile-ast
	(cleavir-ast:array-ast ast)
	(context
	 (list temp1)
	 (list (compile-ast
		(cleavir-ast:index-ast ast)
		(context
		 (list temp2)
		 (list (make-instance ',aref-instruction-class
			 :inputs (list temp1 temp2)
			 :outputs (list temp3)
			 :successors
			 (list (make-instance ',box-instruction-class
				 :inputs (list temp3)
				 :outputs (results context)
				 :successors (successors context)))))
		 (invocation context))))
	 (invocation context))))))

(compile-specialized-aref-ast cleavir-ast:bit-aref-ast
			      cleavir-ir:bit-aref-instruction
			      cleavir-ir:bit-box-instruction)

(compile-specialized-aref-ast cleavir-ast:short-float-aref-ast
			      cleavir-ir:short-float-aref-instruction
			      cleavir-ir:short-float-box-instruction)

(compile-specialized-aref-ast cleavir-ast:single-float-aref-ast
			      cleavir-ir:single-float-aref-instruction
			      cleavir-ir:single-float-box-instruction)

(compile-specialized-aref-ast cleavir-ast:double-float-aref-ast
			      cleavir-ir:double-float-aref-instruction
			      cleavir-ir:double-float-box-instruction)

(compile-specialized-aref-ast cleavir-ast:long-float-aref-ast
			      cleavir-ir:long-float-aref-instruction
			      cleavir-ir:long-float-box-instruction)

(defmacro compile-specialized-aset-ast
    (ast-class aset-instruction-class unbox-instruction-class)
  `(defmethod compile-ast ((ast ,ast-class) context)
     (check-context-for-no-value-ast context)
     (let ((temp1 (make-temp nil))
	   (temp2 (make-temp nil))
	   (temp3 (make-temp nil))
	   (temp4 (make-temp nil)))
       (compile-ast
	(cleavir-ast:array-ast ast)
	(context
	 (list temp1)
	 (list (compile-ast
		(cleavir-ast:index-ast ast)
		(context
		 (list temp2)
		 (compile-ast
		  (cleavir-ast:value-ast ast)
		  (context
		   (list temp3)
		   (list (make-instance ',unbox-instruction-class
			   :inputs (list temp3)
			   :outputs (list temp4)
			   :successors
			   (list (make-instance ',aset-instruction-class
				   :inputs (list temp1 temp2 temp4)
				   :outputs '()
				   :successors (successors context)))))
		   (invocation context)))
		 (invocation context))))
	 (invocation context))))))

(compile-specialized-aset-ast cleavir-ast:bit-aset-ast
			      cleavir-ir:bit-aset-instruction
			      cleavir-ir:bit-unbox-instruction)

(compile-specialized-aset-ast cleavir-ast:short-float-aset-ast
			      cleavir-ir:short-float-aset-instruction
			      cleavir-ir:short-float-unbox-instruction)

(compile-specialized-aset-ast cleavir-ast:single-float-aset-ast
			      cleavir-ir:single-float-aset-instruction
			      cleavir-ir:single-float-unbox-instruction)

(compile-specialized-aset-ast cleavir-ast:double-float-aset-ast
			      cleavir-ir:double-float-aset-instruction
			      cleavir-ir:double-float-unbox-instruction)

(compile-specialized-aset-ast cleavir-ast:long-float-aset-ast
			      cleavir-ir:long-float-aset-instruction
			      cleavir-ir:long-float-unbox-instruction)
