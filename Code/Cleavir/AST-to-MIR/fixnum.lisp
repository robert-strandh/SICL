(cl:in-package #:cleavir-ast-to-hir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-ADD-AST.

(defmethod compile-ast ((ast cleavir-ast:fixnum-add-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (unless (and (= (length results) 1)
		 (= (length successors) 2))
      (error "Invalid context for FIXNUM-ADD-AST."))
    (let ((temp1 (make-temp nil))
	  (temp2 (make-temp nil))
	  (result (find-or-create-location (cleavir-ast:variable-ast ast))))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (context (list temp1)
		(list (compile-ast
		       (cleavir-ast:arg2-ast ast)
		       (context (list temp2)
				(cleavir-hir:make-fixnum-add-instruction
				 (list temp1 temp2)
				 (list result)
				 successors)))))))))

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
    (let ((temp1 (make-temp nil))
	  (temp2 (make-temp nil))
	  (result (find-or-create-location (cleavir-ast:variable-ast ast))))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (context (list temp1)
		(list (compile-ast
		       (cleavir-ast:arg2-ast ast)
		       (context (list temp2)
				(cleavir-hir:make-fixnum-sub-instruction
				 (list temp1 temp2)
				 (list result)
				 successors)))))))))

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
    (let ((temp1 (make-temp nil))
	  (temp2 (make-temp nil)))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (context (list temp1)
		(list (compile-ast
		       (cleavir-ast:arg2-ast ast)
		       (context (list temp2)
				(cleavir-hir:make-fixnum-less-instruction
				 (list temp1 temp2)
				 successors)))))))))

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
    (let ((temp1 (make-temp nil))
	  (temp2 (make-temp nil)))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (context (list temp1)
		(list (compile-ast
		       (cleavir-ast:arg2-ast ast)
		       (context (list temp2)
				(cleavir-hir:make-fixnum-not-greater-instruction
				 (list temp1 temp2)
				 successors)))))))))

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
    (let ((temp1 (make-temp nil))
	  (temp2 (make-temp nil)))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (context (list temp1)
		(list (compile-ast
		       (cleavir-ast:arg2-ast ast)
		       (context (list temp2)
				(cleavir-hir:make-fixnum-less-instruction
				 (list temp2 temp1)
				 successors)))))))))

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
    (let ((temp1 (make-temp nil))
	  (temp2 (make-temp nil)))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (context (list temp1)
		(list (compile-ast
		       (cleavir-ast:arg2-ast ast)
		       (context (list temp2)
				(cleavir-hir:make-fixnum-not-greater-instruction
				 (list temp2 temp1)
				 successors)))))))))

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
    (let ((temp1 (make-temp nil))
	  (temp2 (make-temp nil)))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (context (list temp1)
		(list (compile-ast
		       (cleavir-ast:arg2-ast ast)
		       (context (list temp2)
				(cleavir-hir:make-fixnum-equal-instruction
				 (list temp2 temp1)
				 successors)))))))))
