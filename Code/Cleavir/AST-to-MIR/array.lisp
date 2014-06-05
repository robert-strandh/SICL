(cl:in-package #:cleavir-ast-to-mir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a AREF-AST

(defmethod compile-ast ((ast cleavir-ast:aref-ast) context)
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
		      (list (make-instance 'cleavir-mir:aref-instruction
			      :inputs (list temp1 temp2)
			      :outputs (results context)
			      :successors (successors context))))))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a ASET-AST

(defmethod compile-ast ((ast cleavir-ast:aset-ast) context)
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
		(list (make-instance 'cleavir-mir:aset-instruction
			:inputs (list temp1 temp2 temp3)
			:outputs '()
			:successors (successors context))))))))))))

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
				 :successors (successors context)))))))))))))

(compile-specialized-aref-ast cleavir-ast:short-float-aref-ast
			      cleavir-mir:short-float-aref-instruction
			      cleavir-mir:short-float-box-instruction)

(compile-specialized-aref-ast cleavir-ast:single-float-aref-ast
			      cleavir-mir:single-float-aref-instruction
			      cleavir-mir:single-float-box-instruction)

(compile-specialized-aref-ast cleavir-ast:double-float-aref-ast
			      cleavir-mir:double-float-aref-instruction
			      cleavir-mir:double-float-box-instruction)

(compile-specialized-aref-ast cleavir-ast:long-float-aref-ast
			      cleavir-mir:long-float-aref-instruction
			      cleavir-mir:long-float-box-instruction)

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
				   :successors (successors context)))))))))))))))
