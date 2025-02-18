(cl:in-package #:sicl-ast-to-hir)

(defmethod translate-ast (client (ast ico:literal-ast))
  (let ((literal (ico:literal ast)))
    (make-instance 'hir:assignment-instruction
      :inputs (list (make-instance 'hir:literal :value literal))
      :outputs (list *target-register*)
      :successors (list *next-instruction* ))))
