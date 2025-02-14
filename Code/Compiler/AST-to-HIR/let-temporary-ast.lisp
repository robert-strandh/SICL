(cl:in-package #:sicl-ast-to-hir)

(defmethod translate-ast (client (ast ico:let-temporary-ast) context)
  (let* ((binding-ast (ico:binding-ast ast))
         (variable-definition-ast (ico:variable-definition-ast binding-ast))
         (init-form-ast (ico:form-ast binding-ast))
         (body-form-asts (ico:form-asts ast))
         (register (make-instance 'hir:single-value-register)))
    (setf (find-register variable-definition-ast) register)
    (let ((first-body-instruction
            (translate-implicit-progn client body-form-asts context)))
      (let ((init-form-context
              (make-context 1 first-body-instruction register)))
        (translate-ast client init-form-ast init-form-context)))))
