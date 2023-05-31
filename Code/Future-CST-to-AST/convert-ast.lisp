(cl:in-package #:sicl-expression-to-ast)

(defmethod convert-ast
    (builder (ast ico:unparsed-form-ast))
  (with-builder-components (builder client environment)
    (convert client (ico:form ast) environment)))
