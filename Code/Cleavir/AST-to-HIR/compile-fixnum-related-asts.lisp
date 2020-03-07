(cl:in-package #:cleavir-ast-to-hir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-ADD-AST.

(defmethod compile-ast ((ast cleavir-ast:fixnum-add-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (assert-context ast context 0 2)
    (let ((temp1 (make-temp))
	  (temp2 (make-temp))
	  (result (find-or-create-location (cleavir-ast:variable-ast ast))))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (clone-context
        context
        :results
        (list temp1)
        :successors
        (list (compile-ast
               (cleavir-ast:arg2-ast ast)
               (clone-context
                context
                :results (list temp2)
                :successors (list (cleavir-ir:make-fixnum-add-instruction
                                   (list temp1 temp2)
                                   result
                                   successors))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-SUB-AST.

(defmethod compile-ast ((ast cleavir-ast:fixnum-sub-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (assert-context ast context 0 2)
    (let ((temp1 (make-temp))
	  (temp2 (make-temp))
	  (result (find-or-create-location (cleavir-ast:variable-ast ast))))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (clone-context
        context
        :results (list temp1)
        :successors
        (list (compile-ast
               (cleavir-ast:arg2-ast ast)
               (clone-context
                context
                :results (list temp2)
                :successors
                (list (cleavir-ir:make-fixnum-sub-instruction
                       (list temp1 temp2)
                       result
                       successors))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-LESS-AST.

(defmethod compile-ast ((ast cleavir-ast:fixnum-less-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (assert-context ast context 0 2)
    (let ((temp1 (make-temp))
	  (temp2 (make-temp)))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (clone-context
        context
        :results (list temp1)
        :successors
        (list (compile-ast
               (cleavir-ast:arg2-ast ast)
               (clone-context
                context
                :results (list temp2)
                :successors
                (list (cleavir-ir:make-fixnum-less-instruction
                       (list temp1 temp2)
                       successors))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-NOT-GREATER-AST.

(defmethod compile-ast ((ast cleavir-ast:fixnum-not-greater-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (assert-context ast context 0 2)
    (let ((temp1 (make-temp))
	  (temp2 (make-temp)))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (clone-context
        context
        :results (list temp1)
        :successors
        (list (compile-ast
               (cleavir-ast:arg2-ast ast)
               (clone-context
                context
                :results (list temp2)
                :successors
                (list (cleavir-ir:make-fixnum-not-greater-instruction
                       (list temp1 temp2)
                       successors))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-GREATER-AST.

(defmethod compile-ast ((ast cleavir-ast:fixnum-greater-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (assert-context ast context 0 2)
    (let ((temp1 (make-temp))
	  (temp2 (make-temp)))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (clone-context
        context
        :results (list temp1)
        :successors
        (list (compile-ast
               (cleavir-ast:arg2-ast ast)
               (clone-context
                context
                :results (list temp2)
                :successors
                (list (cleavir-ir:make-fixnum-less-instruction
                       (list temp2 temp1)
                       successors))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-NOT-LESS-AST.

(defmethod compile-ast ((ast cleavir-ast:fixnum-not-less-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (assert-context ast context 0 2)
    (let ((temp1 (make-temp))
	  (temp2 (make-temp)))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (clone-context
        context
        :results (list temp1)
        :successors
        (list (compile-ast
               (cleavir-ast:arg2-ast ast)
               (clone-context
                context
                :results (list temp2)
                :successors
                (list (cleavir-ir:make-fixnum-not-greater-instruction
                       (list temp2 temp1)
                       successors))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-EQUAL-AST.

(defmethod compile-ast ((ast cleavir-ast:fixnum-equal-ast) context)
  (with-accessors ((results results)
		   (successors successors))
      context
    (assert-context ast context 0 2)
    (let ((temp1 (make-temp))
	  (temp2 (make-temp)))
      (compile-ast
       (cleavir-ast:arg1-ast ast)
       (clone-context
        context
        :results (list temp1)
        :successors
        (list (compile-ast
               (cleavir-ast:arg2-ast ast)
               (clone-context
                context
                :results (list temp2)
                :successors
                (list (cleavir-ir:make-fixnum-equal-instruction
                       (list temp2 temp1)
                       successors))))))))))
