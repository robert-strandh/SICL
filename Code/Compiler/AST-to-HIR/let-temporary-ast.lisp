(cl:in-package #:sicl-ast-to-hir)

(defmethod translate-ast (client (ast ico:let-temporary-ast))
  (let* ((binding-ast (ico:binding-ast ast))
         (variable-definition-ast (ico:variable-name-ast binding-ast))
         (init-form-ast (ico:form-ast binding-ast))
         (body-form-asts (ico:form-asts ast))
         (register (make-instance 'hir:single-value-register)))
    (setf (find-register variable-definition-ast) register)
      (let ((*next-instruction*
              (translate-implicit-progn client body-form-asts))
            (*target-register* register))
        (translate-ast client init-form-ast))))
