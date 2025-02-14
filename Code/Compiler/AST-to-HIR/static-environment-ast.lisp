(cl:in-package #:sicl-ast-to-hir)

(defmethod translate-ast (client (ast ico:static-environment-ast))
  (make-instance 'hir:static-environment-instruction
    :inputs '()
    :outputs (list *target-register*)
    :successors (list *next-instruction*)))
