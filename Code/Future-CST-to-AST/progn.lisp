(cl:in-package #:sicl-future-cst-to-ast)

(defmethod abp:finish-node
    ((builder builder)
     (kind t)
     (ast ico:progn-ast))
  (reinitialize-instance ast
    :form-asts
    (loop for body-ast in (ico:form-asts ast)
          for cst in (ico:origin body-ast)
          collect (convert (client builder) cst (environment builder)))))
