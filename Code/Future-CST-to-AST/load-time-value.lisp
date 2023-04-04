(cl:in-package #:sicl-expression-to-ast)

(defmethod abp:finish-node
    ((builder builder)
     (kind t)
     (ast ico:load-time-value-ast))
  (with-builder-components (builder client environment)
    (let* ((global-environment
             (trucler:global-environment client environment))
           (new-builder (make-builder client global-environment)))
      (reinitialize-instance ast
        :form-ast (convert-ast new-builder (ico:form-ast ast))))))

