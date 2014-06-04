(cl:in-package #:cleavir-ast-to-mir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a CAR-AST

(defmethod compile-ast ((ast cleavir-ast:car-ast) context)
  (check-context-for-one-value-ast context)
  (let ((temp (make-temp nil)))
    (compile-ast
     (cleavir-ast:cons-ast ast)
     (context (list temp)
	      (list (make-instance 'cleavir-mir:car-instruction
		      :inputs (list temp)
		      :outputs (results context)
		      :successors (successors context)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a CDR-AST

(defmethod compile-ast ((ast cleavir-ast:cdr-ast) context)
  (check-context-for-one-value-ast context)
  (let ((temp (make-temp nil)))
    (compile-ast
     (cleavir-ast:cons-ast ast)
     (context (list temp)
	      (list (make-instance 'cleavir-mir:cdr-instruction
		      :inputs (list temp)
		      :outputs (results context)
		      :successors (successors context)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a RPLACA-AST

(defmethod compile-ast ((ast cleavir-ast:rplaca-ast) context)
  (check-context-for-no-value-ast context)
  (let ((temp1 (make-temp nil))
	(temp2 (make-temp nil)))
    (compile-ast
     (cleavir-ast:cons-ast ast)
     (context
      (list temp1)
      (list (compile-ast
	     (cleavir-ast:object-ast ast)
	     (context (list temp2)
		      (list (make-instance 'cleavir-mir:rplaca-instruction
			      :inputs (list temp1 temp2)
			      :outputs '()
			      :successors (successors context))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a RPLACD-AST

(defmethod compile-ast ((ast cleavir-ast:rplacd-ast) context)
  (check-context-for-no-value-ast context)
  (let ((temp1 (make-temp nil))
	(temp2 (make-temp nil)))
    (compile-ast
     (cleavir-ast:cons-ast ast)
     (context
      (list temp1)
      (list (compile-ast
	     (cleavir-ast:object-ast ast)
	     (context (list temp2)
		      (list (make-instance 'cleavir-mir:rplacd-instruction
			      :inputs (list temp1 temp2)
			      :outputs '()
			      :successors (successors context))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a SLOT-READ-AST

(defmethod compile-ast ((ast cleavir-ast:slot-read-ast) context)
  (check-context-for-one-value-ast context)
  (let ((temp1 (make-temp nil))
	(temp2 (make-temp nil)))
    (compile-ast
     (cleavir-ast:object-ast ast)
     (context
      (list temp1)
      (list (compile-ast
	     (cleavir-ast:slot-number-ast ast)
	     (context (list temp2)
		      (list (make-instance 'cleavir-mir:slot-read-instruction
			      :inputs (list temp1 temp2)
			      :outputs (results context)
			      :successors (successors context))))))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a SLOT-WRITE-AST

(defmethod compile-ast ((ast cleavir-ast:slot-write-ast) context)
  (check-context-for-no-value-ast context)
  (let ((temp1 (make-temp nil))
	(temp2 (make-temp nil))
	(temp3 (make-temp nil)))
    (compile-ast
     (cleavir-ast:object-ast ast)
     (context
      (list temp1)
      (list (compile-ast
	     (cleavir-ast:slot-number-ast ast)
	     (context
	      (list temp2)
	      (compile-ast
	       (cleavir-ast:value-ast ast)
	       (context
		(list temp3)
		(list (make-instance 'cleavir-mir:slot-write-instruction
			:inputs (list temp1 temp2 temp3)
			:outputs '()
			:successors (successors context))))))))))))
