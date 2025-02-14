(cl:in-package #:sicl-ast-to-hir)

(defmethod translate-ast (client (ast ico:variable-reference-ast) context)
  (with-context-components (context)
    (let* ((variable-definition-ast (ico:definition-ast ast))
           (register (find-register variable-definition-ast)))
      (make-instance 'hir:assignment-instruction
        :inputs (list register)
        :outputs (list target-register)
        :successors (list next-instruction)))))
