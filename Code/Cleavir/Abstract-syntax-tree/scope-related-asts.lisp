(cl:in-package #:cleavir-ast)

(defclass scope-ast (ast)
  ((%child-ast :initarg :child-ast :reader child-ast)
   (%variable-ast :initarg :variable-ast :reader variable-ast)))

(cleavir-io:define-save-info scope-ast
  (:child-ast child-ast)
  (:variable-ast variagble-ast))
