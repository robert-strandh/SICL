(cl:in-package #:cleavir-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SCOPE-AST.
;;;
;;; This class is generated when a scoping construct introducing a
;;; lexical variable (LET, LET*, FLET, LABELS) is seen in source code.
;;; It indicates that the variable is in scope for the execution of
;;; the child AST.
;;;
;;; When the DEBUG quality has a high value, code may be generated to
;;; keep the variable alive artificially beyond what a data-flow
;;; analysis would indicate.  As a result, the programmer could then
;;; interact with the values of this variable even though it may no
;;; longer be alive as far as the semantics of the source code is
;;; concerned.

(defclass scope-ast (ast)
  ((%child-ast :initarg :child-ast :reader child-ast)
   (%variable-ast :initarg :variable-ast :reader variable-ast)))

(cleavir-io:define-save-info scope-ast
  (:child-ast child-ast)
  (:variable-ast variable-ast))
