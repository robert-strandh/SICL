(cl:in-package #:sicl-expression-to-ast)

(defmethod abp:finish-node
    ((builder builder)
     (kind t)
     (ast ico:unwind-protect-ast))
  (reinitialize-instance ast
    :protected-form-ast (convert-ast builder (ico:protected-form-ast ast))
    :form-asts (loop for cleanup-form-ast in (ico:form-asts ast)
                     collect (convert-ast builder cleanup-form-ast))))
