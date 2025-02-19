(cl:in-package #:sicl-ast-to-hir)

(defmethod translate-ast
    (client (ast ico:global-function-name-reference-ast))
  (make-instance 'hir:global-function-reference-instruction
    :function-name (ico:name ast)
    :outputs (list *target-register*)
    :successors (list *next-instruction*)))
