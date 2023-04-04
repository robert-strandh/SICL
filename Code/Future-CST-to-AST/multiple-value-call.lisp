(cl:in-package #:sicl-expression-to-ast)

(defmethod abp:finish-node
    ((builder builder)
     (kind t)
     (ast ico:multiple-value-call-ast))
  (reinitialize-instance ast
    :function-form-ast (convert-ast builder (ico:function-form-ast ast))
    :form-asts
    (loop for body-ast in (ico:form-asts ast)
          collect (convert-ast builder body-ast))))
