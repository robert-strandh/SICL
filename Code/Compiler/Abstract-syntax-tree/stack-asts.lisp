(cl:in-package #:sicl-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CALLER-STACK-POINTER-AST

(defclass caller-stack-pointer-ast
    (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ())

(cleavir-io:define-save-info caller-stack-pointer-ast)

(defmethod cleavir-ast:children ((ast caller-stack-pointer-ast))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CALLER-FRAME-POINTER-AST

(defclass caller-frame-pointer-ast
    (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ())

(cleavir-io:define-save-info caller-frame-pointer-ast)

(defmethod cleavir-ast:children ((ast caller-frame-pointer-ast))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ESTABLISH-STACK-FRAME-AST

(defclass establish-stack-frame-ast
    (cleavir-ast:ast cleavir-ast:no-value-ast-mixin)
  ((%stack-pointer-ast :initarg :stack-pointer-ast :reader stack-pointer-ast)
   (%frame-pointer-ast :initarg :frame-pointer-ast :reader frame-pointer-ast)))

(cleavir-io:define-save-info establish-stack-frame-ast)

(defmethod cleavir-ast:children ((ast establish-stack-frame-ast))
  (list (stack-pointer-ast ast)
        (frame-pointer-ast ast)))
