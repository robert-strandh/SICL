(cl:in-package #:cleavir-ast-to-hir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUMP-AST.

(defmethod compile-ast (client (ast cleavir-ast:fixnump-ast) context)
  (assert-context ast context 0 2)
  (let ((temp (make-temp)))
    (compile-ast
     client
     (cleavir-ast:object-ast ast)
     (clone-context
      context
      :result temp
      :successor
      (make-instance 'cleavir-ir:fixnump-instruction
        :input temp
        :successors (successors context))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-ADD-AST.

(defmethod compile-ast (client (ast cleavir-ast:fixnum-add-ast) context)
  (assert-context ast context 0 2)
  (let ((temp1 (make-temp))
        (temp2 (make-temp))
        (result (find-or-create-location (cleavir-ast:variable-ast ast))))
    (compile-ast
     client
     (cleavir-ast:arg1-ast ast)
     (clone-context
      context
      :result temp1
      :successor
      (compile-ast
       client
       (cleavir-ast:arg2-ast ast)
       (clone-context
        context
        :result temp2
        :successor
        (make-instance 'cleavir-ir:fixnum-add-instruction
          :inputs (list temp1 temp2)
          :output result
          :successors (successors context))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-SUB-AST.

(defmethod compile-ast (client (ast cleavir-ast:fixnum-sub-ast) context)
  (assert-context ast context 0 2)
  (let ((temp1 (make-temp))
        (temp2 (make-temp))
        (result (find-or-create-location (cleavir-ast:variable-ast ast))))
    (compile-ast
     client
     (cleavir-ast:arg1-ast ast)
     (clone-context
      context
      :result  temp1
      :successor (compile-ast
                  client
                  (cleavir-ast:arg2-ast ast)
                  (clone-context
                   context
                   :result temp2
                   :successor (make-instance 'cleavir-ir:fixnum-sub-instruction
                                :inputs (list temp1 temp2)
                                :output result
                                :successors (successors context))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-DIVIDE-AST.

(defmethod compile-ast (client (ast cleavir-ast:fixnum-divide-ast) context)
  (assert-context ast context 2 1)
  (let ((temp1 (make-temp))
        (temp2 (make-temp)))
    (compile-ast
     client
     (cleavir-ast:dividend-ast ast)
     (clone-context
      context
      :result temp1
      :successor (compile-ast
                  client
                  (cleavir-ast:divisor-ast ast)
                  (clone-context
                   context
                   :result temp2
                   :successor (make-instance 'cleavir-ir:fixnum-divide-instruction
                                :inputs (list temp1 temp2)
                                :outputs (results context)
                                :successors (successors context))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-MULTIPLY-AST.

(defmethod compile-ast (client (ast cleavir-ast:fixnum-multiply-ast) context)
  (assert-context ast context 2 1)
  (let ((temp1 (make-temp))
        (temp2 (make-temp)))
    (compile-ast
     client
     (cleavir-ast:multiplier-ast ast)
     (clone-context
      context
      :result temp1
      :successor (compile-ast
                  client
                  (cleavir-ast:multiplicand-ast ast)
                  (clone-context
                   context
                   :result temp2
                   :successor (make-instance 'cleavir-ir:fixnum-multiply-instruction
                                :inputs (list temp1 temp2)
                                :outputs (results context)
                                :successors (successors context))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-LESS-AST.

(defmethod compile-ast (client (ast cleavir-ast:fixnum-less-ast) context)
  (assert-context ast context 0 2)
  (let ((temp1 (make-temp))
        (temp2 (make-temp)))
    (compile-ast
     client
     (cleavir-ast:arg1-ast ast)
     (clone-context
      context
      :result temp1
      :successor (compile-ast
                  client
                  (cleavir-ast:arg2-ast ast)
                  (clone-context
                   context
                   :result temp2
                   :successor (make-instance 'cleavir-ir:fixnum-less-instruction
                                :inputs (list temp1 temp2)
                                :successors (successors context))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-NOT-GREATER-AST.

(defmethod compile-ast (client (ast cleavir-ast:fixnum-not-greater-ast) context)
  (assert-context ast context 0 2)
  (let ((temp1 (make-temp))
        (temp2 (make-temp)))
    (compile-ast
     client
     (cleavir-ast:arg1-ast ast)
     (clone-context
      context
      :result temp1
      :successor (compile-ast
                  client
                  (cleavir-ast:arg2-ast ast)
                  (clone-context
                   context
                   :result temp2
                   :successor (make-instance 'cleavir-ir:fixnum-not-greater-instruction
                                :inputs (list temp1 temp2)
                                :successors (successors context))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-GREATER-AST.

(defmethod compile-ast (client (ast cleavir-ast:fixnum-greater-ast) context)
  (assert-context ast context 0 2)
  (let ((temp1 (make-temp))
        (temp2 (make-temp)))
    (compile-ast
     client
     (cleavir-ast:arg1-ast ast)
     (clone-context
      context
      :result temp1
      :successor (compile-ast
                  client
                  (cleavir-ast:arg2-ast ast)
                  (clone-context
                   context
                   :result temp2
                   :successor (make-instance 'cleavir-ir:fixnum-less-instruction
                                :inputs (list temp2 temp1)
                                :successors (successors context))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-NOT-LESS-AST.

(defmethod compile-ast (client (ast cleavir-ast:fixnum-not-less-ast) context)
  (assert-context ast context 0 2)
  (let ((temp1 (make-temp))
        (temp2 (make-temp)))
    (compile-ast
     client
     (cleavir-ast:arg1-ast ast)
     (clone-context
      context
      :result temp1
      :successor (compile-ast
                  client
                  (cleavir-ast:arg2-ast ast)
                  (clone-context
                   context
                   :result temp2
                   :successor (make-instance 'cleavir-ir:fixnum-not-greater-instruction
                                :inputs (list temp2 temp1)
                                :successors (successors context))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-EQUAL-AST.

(defmethod compile-ast (client (ast cleavir-ast:fixnum-equal-ast) context)
  (assert-context ast context 0 2)
  (let ((temp1 (make-temp))
        (temp2 (make-temp)))
    (compile-ast
     client
     (cleavir-ast:arg1-ast ast)
     (clone-context
      context
      :result temp1
      :successor  (compile-ast
                   client
                   (cleavir-ast:arg2-ast ast)
                   (clone-context
                    context
                    :result temp2
                    :successor (make-instance 'cleavir-ir:fixnum-equal-instruction
                                 :inputs (list temp2 temp1)
                                 :successors (successors context))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-LOGAND-AST

(define-compile-functional-ast
    cleavir-ast:fixnum-logand-ast cleavir-ir:fixnum-logand-instruction
  (cleavir-ast:arg1-ast cleavir-ast:arg2-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-LOGIOR-AST

(define-compile-functional-ast
    cleavir-ast:fixnum-logior-ast cleavir-ir:fixnum-logior-instruction
  (cleavir-ast:arg1-ast cleavir-ast:arg2-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-LOGXOR-AST

(define-compile-functional-ast
    cleavir-ast:fixnum-logxor-ast cleavir-ir:fixnum-logxor-instruction
  (cleavir-ast:arg1-ast cleavir-ast:arg2-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a FIXNUM-LOGNOT-AST

(define-compile-functional-ast
    cleavir-ast:fixnum-lognot-ast cleavir-ir:fixnum-lognot-instruction
  (cleavir-ast:arg-ast))
