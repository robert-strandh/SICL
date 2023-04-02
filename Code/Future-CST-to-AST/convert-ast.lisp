(cl:in-package #:sicl-expression-to-ast)

(defmethod convert-ast
    (builder (ast bld:unparsed-form-ast))
  (with-builder-components (builder client environment)
    (convert client (bld:form ast) environment)))
