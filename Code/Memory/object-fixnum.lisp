(cl:in-package #:sicl-memory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OBJECT-TO-FIXNUM

(defclass object-to-fixnum-ast
    (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((%object-ast :initarg :object-ast :reader object-ast)))

(cleavir-io:define-save-info object-to-fixnum-ast
  (:object-ast object-ast))

(defmethod cleavir-ast:children ((ast object-to-fixnum-ast))
  (list (object-ast ast)))

(defmethod cleavir-cst-to-ast:convert-special
    (client (symbol (eql 'object-to-fixnum)) cst environment)
  (cleavir-cst-to-ast:check-simple-primop-syntax cst 1)
  (cst:db origin (object-to-fixnum-cst object-cst) cst
    (declare (ignore object-to-fixnum-cst))
    (cleavir-ast:make-ast 'object-to-fixnum-ast
      :object-ast
      (cleavir-cst-to-ast:convert client object-cst environment))))

(defclass object-to-fixnum-instruction
    (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ())

(cleavir-ast-to-hir:define-compile-functional-ast
    object-to-fixnum-ast object-to-fixnum-instruction
  (object-ast))

(defmethod sicl-hir-to-mir:process-instruction
    (client (instruction object-to-fixnum-instruction))
  (change-class instruction 'cleavir-ir:shift-left-instruction
                :inputs
                (list (cleavir-ir:inputs instruction)
                      (make-instance 'cleavir-ir:immediate-input
                        :value 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FIXNUM-TO-OBJECT

(defclass fixnum-to-object-ast
    (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((%fixnum-ast :initarg :fixnum-ast :reader fixnum-ast)))

(cleavir-io:define-save-info fixnum-to-object-ast
  (:fixnum-ast fixnum-ast))

(defmethod cleavir-ast:children ((ast fixnum-to-object-ast))
  (list (fixnum-ast ast)))

(defmethod cleavir-cst-to-ast:convert-special
    (client (symbol (eql 'fixnum-to-object)) cst environment)
  (cleavir-cst-to-ast:check-simple-primop-syntax cst 1)
  (cst:db origin (fixnum-to-object-cst fixnum-cst) cst
    (declare (ignore fixnum-to-object-cst))
    (cleavir-ast:make-ast 'fixnum-to-object-ast
      :fixnum-ast
      (cleavir-cst-to-ast:convert client fixnum-cst environment))))

(defclass fixnum-to-object-instruction
    (cleavir-ir:instruction cleavir-ir:one-successor-mixin)
  ())

(cleavir-ast-to-hir:define-compile-functional-ast
    fixnum-to-object-ast fixnum-to-object-instruction
  (fixnum-ast))

(defmethod sicl-hir-to-mir:process-instruction
    (client (instruction fixnum-to-object-instruction))
  (change-class instruction 'cleavir-ir:logic-shift-right-instruction
                :inputs
                (list (cleavir-ir:inputs instruction)
                      (make-instance 'cleavir-ir:immediate-input
                        :value 1))))
