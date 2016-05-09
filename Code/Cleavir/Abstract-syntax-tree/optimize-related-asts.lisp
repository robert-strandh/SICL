(cl:in-package #:cleavir-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class OPTIMIZE-AST.

(defclass optimize-ast (ast)
  ((%child-ast :initarg :child-ast :reader child-ast)))

(defun make-optimize-ast (child-ast &key origin)
  (make-instance 'optimize-ast
    :origin origin
    :child-ast child-ast))

(cleavir-io:define-save-info optimize-ast
  (:child-ast child-ast))

(defmethod children ((ast optimize-ast))
  (list (child-ast ast)))
