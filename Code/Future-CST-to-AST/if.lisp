(cl:in-package #:sicl-expression-to-ast)

(defmethod abp:finish-node
    ((builder builder)
     (kind t)
     (ast ico:if-ast))
  (reinitialize-instance ast
    :test-ast (convert-ast builder (ico:test-ast ast))
    :then-ast (convert-ast builder (ico:then-ast ast))
    :else-ast (convert-ast builder (ico:else-ast ast))))
