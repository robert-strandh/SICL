(cl:in-package #:sicl-ast-to-hir)

(defmethod translate-ast (client (ast ico:go-with-variable-ast))
  (let* ((variable-reference-ast (ico:variable-reference-ast ast))
         (definition-ast (ico:definition-ast variable-reference-ast))
         (index-ast (ico:index-ast ast))
         (index (ico:literal index-ast))
         (identity-register (find-register definition-ast))
         (tagbody-vector (assoc definition-ast *tagbody-vectors*)))
    (make-instance 'hir:unwind-instruction
      :inputs (list *dynamic-environment-register* identity-register)
      :outputs '()
      ;; We do not necessarily know the successor at this point, so
      ;; instead we stick the index and the tagbody vector in the
      ;; SUCCESSORS slot and we fix things up later.
      :successors (list index tagbody-vector))))
