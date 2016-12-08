(cl:in-package #:cleavir-ast-to-hir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a CAR-AST

(defmethod compile-ast ((ast cleavir-ast:car-ast) context)
  (let ((temp (make-temp)))
    (compile-ast
     (cleavir-ast:cons-ast ast)
     (context (list temp)
	      (list (make-instance 'cleavir-ir:car-instruction
		      :inputs (list temp)
		      :outputs (results context)
		      :successors (successors context)))
	      (invocation context)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a CDR-AST

(defmethod compile-ast ((ast cleavir-ast:cdr-ast) context)
  (let ((temp (make-temp)))
    (compile-ast
     (cleavir-ast:cons-ast ast)
     (context (list temp)
	      (list (make-instance 'cleavir-ir:cdr-instruction
		      :inputs (list temp)
		      :outputs (results context)
		      :successors (successors context)))
	      (invocation context)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a RPLACA-AST

(defmethod compile-ast ((ast cleavir-ast:rplaca-ast) context)
  (let ((temp1 (make-temp))
	(temp2 (make-temp)))
    (compile-ast
     (cleavir-ast:cons-ast ast)
     (context
      (list temp1)
      (list (compile-ast
	     (cleavir-ast:object-ast ast)
	     (context (list temp2)
		      (list (make-instance 'cleavir-ir:rplaca-instruction
			      :inputs (list temp1 temp2)
			      :outputs '()
			      :successors (successors context)))
		      (invocation context))))
      (invocation context)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a RPLACD-AST

(defmethod compile-ast ((ast cleavir-ast:rplacd-ast) context)
  (let ((temp1 (make-temp))
	(temp2 (make-temp)))
    (compile-ast
     (cleavir-ast:cons-ast ast)
     (context
      (list temp1)
      (list (compile-ast
	     (cleavir-ast:object-ast ast)
	     (context (list temp2)
		      (list (make-instance 'cleavir-ir:rplacd-instruction
			      :inputs (list temp1 temp2)
			      :outputs '()
			      :successors (successors context)))
		      (invocation context))))
      (invocation context)))))
