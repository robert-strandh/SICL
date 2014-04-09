(cl:in-package #:cleavir-ast-to-mir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a CAR-AST

(defmethod compile-ast ((ast cleavir-ast:car-ast) context)
  (multiple-value-bind (successor result)
      (adapt-context-1-1 context)
    (let* ((temp (make-temp nil))
	   (succ (cleavir-mir:make-car-instruction temp result successor))
	   (new-context (context (list temp) (list succ))))
      (compile-ast (cleavir-ast:cons-ast ast) new-context))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a CDR-AST

(defmethod compile-ast ((ast cleavir-ast:cdr-ast) context)
  (multiple-value-bind (successor result)
      (adapt-context-1-1 context)
    (let* ((temp (make-temp nil))
	   (succ (cleavir-mir:make-cdr-instruction temp result successor))
	   (new-context (context (list temp) (list succ))))
      (compile-ast (cleavir-ast:cons-ast ast) new-context))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a RPLACA-AST

(defmethod compile-ast ((ast cleavir-ast:rplaca-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (unless (and (null results) (= (length successors) 1))
      (error "Attempt to compile a RPLACA-AST in a context requiring a value."))
    (let* ((succ (car successors))
	   (temp1 (make-temp nil))
	   (temp2 (make-temp nil))
	   (next (cleavir-mir:make-rplaca-instruction temp1 temp2 succ)))
      (compile-ast
       (cleavir-ast:cons-ast ast)
       (context
	(list temp1)
	(list (compile-ast
	       (cleavir-ast:object-ast ast)
	       (context (list temp2) (list next)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a RPLACD-AST

(defmethod compile-ast ((ast cleavir-ast:rplacd-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (unless (and (null results) (= (length successors) 1))
      (error "Attempt to compile a RPLACD-AST in a context requiring a value."))
    (let* ((succ (car successors))
	   (temp1 (make-temp nil))
	   (temp2 (make-temp nil))
	   (next (cleavir-mir:make-rplacd-instruction temp1 temp2 succ)))
      (compile-ast
       (cleavir-ast:cons-ast ast)
       (context
	(list temp1)
	(list (compile-ast
	       (cleavir-ast:object-ast ast)
	       (context (list temp2) (list next)))))))))
