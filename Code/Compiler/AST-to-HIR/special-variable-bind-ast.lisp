(cl:in-package #:sicl-ast-to-hir)

(defmethod translate-ast (client (ast ico:special-variable-bind-ast))
  (let* ((binding-ast (ico:binding-ast ast))
         (form-ast (ico:form-ast binding-ast))
         (variable-name-ast (ico:variable-name-ast binding-ast))
         (current-dynamic-environment-register
           *dynamic-environment-register*)
         (*dynamic-environment-register*
           (make-instance 'hir:single-value-register))
         (value-register (make-instance 'hir:single-value-register))
         (name-register (make-instance 'hir:single-value-register))
         (*next-instruction*
           (make-instance 'hir:special-variable-binding-instruction
             :inputs (list current-dynamic-environment-register
                           name-register
                           value-register)
             :outputs (list *dynamic-environment-register*)
             :successors (list *next-instruction*)))
         (*target-register* name-register)
         (*values-count* 1)
         (*next-instruction* (translate-ast client variable-name-ast))
         (*target-register* value-register))
    (translate-ast client form-ast)))
