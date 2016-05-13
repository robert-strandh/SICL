(cl:in-package #:cleavir-ast)

(defclass scope-ast (ast)
  ((%child-ast :initarg :child-ast :reader child-ast)
   (%variable-ast :initarg :variable-ast :reader variable-ast)))

(cleavir-io:define-save-info scope-ast
  (:child-ast child-ast)
  (:variable-ast variable-ast))

(defun make-scope-ast (variable-ast child-ast &key origin)
  (make-instance 'scope-east
    :origin origin
    :variable-ast variable-ast
    :child-ast child-ast))
