(cl:in-package #:sicl-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CALLER-STACK-POINTER-AST

(defclass caller-stack-pointer-ast
    (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ())

(clonedijk:define-clone-information caller-stack-pointer-ast)

(defmethod cleavir-ast:children ((ast caller-stack-pointer-ast))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CALLER-FRAME-POINTER-AST

(defclass caller-frame-pointer-ast
    (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ())

(clonedijk:define-clone-information caller-frame-pointer-ast)

(defmethod cleavir-ast:children ((ast caller-frame-pointer-ast))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ESTABLISH-STACK-FRAME-AST

(defclass establish-stack-frame-ast
    (cleavir-ast:ast cleavir-ast:no-value-ast-mixin)
  ((%stack-pointer-ast :initarg :stack-pointer-ast :reader stack-pointer-ast)
   (%frame-pointer-ast :initarg :frame-pointer-ast :reader frame-pointer-ast)))

(clonedijk:define-clone-information establish-stack-frame-ast)

(defmethod cleavir-ast:children ((ast establish-stack-frame-ast))
  (list (stack-pointer-ast ast)
        (frame-pointer-ast ast)))
