(cl:in-package #:cleavir-ast-function-tree)

(defclass node ()
  ((%function-ast :initarg :function-ast :reader function-ast)
   (%parent :initarg :parent :reader parent)
   (%children :initform '() :accessor children)))

(defvar *visited*)

(defgeneric traverse (client ast root-node))

(defmethod traverse :around (client ast root-node)
  (unless (gethash ast *visited*)
    (setf (gethash ast *visited*) t)
    (call-next-method)))

(defmethod traverse (client ast root-node)
  (loop for child in (cleavir-ast:children ast)
        do (traverse client child root-node)))

(defmethod traverse (client (ast cleavir-ast:function-ast) root-node)
  (let ((node (make-instance 'node
                :function-ast ast
                :parent root-node)))
    (push node (children root-node))
    (traverse client (cleavir-ast:body-ast ast) node)))
