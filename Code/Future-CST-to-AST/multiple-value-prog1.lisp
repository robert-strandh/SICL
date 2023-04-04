(cl:in-package #:sicl-expression-to-ast)

(defmethod abp:finish-node
    ((builder builder)
     (kind t)
     (ast ico:multiple-value-prog1-ast))
  (reinitialize-instance ast
    :first-form-ast (convert-ast builder (ico:first-form-ast ast))
    :form-asts
    (loop for body-ast in (ico:form-asts ast)
          collect (convert-ast builder body-ast))))
