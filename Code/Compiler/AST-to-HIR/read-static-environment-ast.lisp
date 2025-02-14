(cl:in-package #:sicl-ast-to-hir)

(defmethod translate-ast (client (ast ico:read-static-environment-ast))
  (let ((static-environment-ast (ico:static-environment-ast ast))
        (index-ast (ico:index-ast ast)))
    (check-type index-ast ico:literal-ast)
    (let* ((literal
             (make-instance 'hir:literal :value (ico:literal index-ast)))
           (register
             (make-instance 'hir:single-value-register))
           (*next-instruction*
             (make-instance 'hir:read-static-environment-instruction
               :inputs (list register literal)
               :outputs (list *target-register*)
               :successors (list *next-instruction*))))
      (translate-ast client static-environment-ast))))
