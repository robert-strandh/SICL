(cl:in-package #:cleavir-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class OPTIMIZE-AST.

(defclass optimize-ast (ast)
  ((%child-ast :initarg :child-ast :reader child-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

(cleavir-io:define-save-info optimize-ast
  (:child-ast child-ast)
  (:value-ast value-ast))

(defmethod children ((ast optimize-ast))
  (list (child-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SPEED-AST.

(defclass speed-ast (optimize-ast)
  ())

(defun make-speed-ast (child-ast value-ast &key origin)
  (make-instance 'speed-ast
    :origin origin
    :child-ast child-ast
    :value-asts value-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DEBUG-AST.

(defclass debug-ast (optimize-ast)
  ())

(defun make-debug-ast (child-ast value-ast &key origin)
  (make-instance 'debug-ast
    :origin origin
    :child-ast child-ast
    :value-asts value-ast))
