(cl:in-package #:sicl-expression-to-ast)

(defmethod abp:finish-node
    ((builder builder)
     (kind t)
     (ast ico:the-ast))
  (reinitialize-instance ast
    :form-ast (convert-ast builder (ico:form-ast ast))))
