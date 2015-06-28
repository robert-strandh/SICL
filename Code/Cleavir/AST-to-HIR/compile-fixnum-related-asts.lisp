(cl:in-package #:cleavir-ast-to-hir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-ADD-AST.

(defmethod compile-ast ((ast cleavir-ast:fixnum-add-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (unless (= (length successors) 2)
      (error "Invalid context for FIXNUM-ADD-AST."))
    (let ((temp1 (make-temp))
	  (temp2 (make-temp))
	  (result (find-or-create-location (cleavir-ast:variable-ast ast))))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (context (list temp1)
		(list (compile-ast
		       (cleavir-ast:arg2-ast ast)
		       (context (list temp2)
				(list (cleavir-ir:make-fixnum-add-instruction
				       (list temp1 temp2)
				       result
				       successors))
				(invocation context))))
		(invocation context))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-SUB-AST.

(defmethod compile-ast ((ast cleavir-ast:fixnum-sub-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (unless (and (= (length results) 1)
		 (= (length successors) 2))
      (error "Invalid context for FIXNUM-SUB-AST."))
    (let ((temp1 (make-temp))
	  (temp2 (make-temp))
	  (result (find-or-create-location (cleavir-ast:variable-ast ast))))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (context (list temp1)
		(list (compile-ast
		       (cleavir-ast:arg2-ast ast)
		       (context (list temp2)
				(cleavir-ir:make-fixnum-sub-instruction
				 (list temp1 temp2)
				 (list result)
				 successors)
				(invocation context))))
		(invocation context))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-LESS-AST.

(defmethod compile-ast ((ast cleavir-ast:fixnum-less-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (unless (and (null results)
		 (= (length successors) 2))
      (error "Invalid context for FIXNUM-LESS-AST."))
    (let ((temp1 (make-temp))
	  (temp2 (make-temp)))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (context (list temp1)
		(list (compile-ast
		       (cleavir-ast:arg2-ast ast)
		       (context (list temp2)
				(cleavir-ir:make-fixnum-less-instruction
				 (list temp1 temp2)
				 successors)
				(invocation context))))
		(invocation context))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-NOT-GREATER-AST.

(defmethod compile-ast ((ast cleavir-ast:fixnum-not-greater-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (unless (and (null results)
		 (= (length successors) 2))
      (error "Invalid context for FIXNUM-NOT-GREATER-AST."))
    (let ((temp1 (make-temp))
	  (temp2 (make-temp)))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (context (list temp1)
		(list (compile-ast
		       (cleavir-ast:arg2-ast ast)
		       (context (list temp2)
				(cleavir-ir:make-fixnum-not-greater-instruction
				 (list temp1 temp2)
				 successors)
				(invocation context))))
		(invocation context))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-GREATER-AST.

(defmethod compile-ast ((ast cleavir-ast:fixnum-greater-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (unless (and (null results)
		 (= (length successors) 2))
      (error "Invalid context for FIXNUM-GREATER-AST."))
    (let ((temp1 (make-temp))
	  (temp2 (make-temp)))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (context (list temp1)
		(list (compile-ast
		       (cleavir-ast:arg2-ast ast)
		       (context (list temp2)
				(cleavir-ir:make-fixnum-less-instruction
				 (list temp2 temp1)
				 successors)
				(invocation context))))
		(invocation context))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-NOT-LESS-AST.

(defmethod compile-ast ((ast cleavir-ast:fixnum-not-less-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (unless (and (null results)
		 (= (length successors) 2))
      (error "Invalid context for FIXNUM-NOT-LESS-AST."))
    (let ((temp1 (make-temp))
	  (temp2 (make-temp)))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (context (list temp1)
		(list (compile-ast
		       (cleavir-ast:arg2-ast ast)
		       (context (list temp2)
				(cleavir-ir:make-fixnum-not-greater-instruction
				 (list temp2 temp1)
				 successors)
				(invocation context))))
		(invocation context))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-EQUAL-AST.

(defmethod compile-ast ((ast cleavir-ast:fixnum-equal-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (unless (and (null results)
		 (= (length successors) 2))
      (error "Invalid context for FIXNUM-EQUAL-AST."))
    (let ((temp1 (make-temp))
	  (temp2 (make-temp)))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (context (list temp1)
		(list (compile-ast
		       (cleavir-ast:arg2-ast ast)
		       (context (list temp2)
				(cleavir-ir:make-fixnum-equal-instruction
				 (list temp2 temp1)
				 successors)
				(invocation context))))
		(invocation context))))))
