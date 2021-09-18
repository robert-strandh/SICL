(cl:in-package #:sicl-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class PATCH-LITERAL-AST.
;;;
;;; This AST class is created by HOIST-LOAD-TIME-VALUES.  The AST that
;;; creates the load-time value becomes the LITERAL-AST of this AST.
;;; The CODE-VECTOR-INDEX-AST and the LITERALS-VECTOR-INDEX-AST are
;;; instance of LOAD-LITERAL-AST that will generate the indices into
;;; the code vector and the literals vector where the load-time value
;;; will be stored.

(defclass patch-literal-ast
    (cleavir-ast:ast cleavir-ast:no-value-ast-mixin)
  ((%literal-ast :initarg :literal-ast :reader literal-ast)
   (%code-vector-index-ast
    :initarg :code-vector-index-ast
    :reader code-vector-index-ast)
   (%literals-vector-index-ast
    :initarg :literals-vector-index-ast
    :reader literals-vector-index-ast)
   (%literal-cell :initarg :literal-cell :reader literal-cell)))
