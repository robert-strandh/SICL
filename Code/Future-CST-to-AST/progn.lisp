(cl:in-package #:sicl-expression-to-ast)

(defmethod abp:finish-node
    ((builder builder)
     (kind t)
     (ast ico:progn-ast))
  (with-builder-components (builder client environment)
    (reinitialize-instance ast
      :form-asts
      (loop for body-ast in (ico:form-asts ast)
            for cst in (ico:origin body-ast)
            collect (convert client cst environment)))))
