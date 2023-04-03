(cl:in-package #:sicl-expression-to-ast)

(defmethod abp:finish-node
    ((builder builder)
     (kind t)
     (ast ico:block-ast))
  (with-builder-components (builder client environment)
    (let* ((block-name-ast (ico:name-ast ast))
           (name (ico:name block-name-ast))
           (new-environment
             (trucler:add-block
              client environment name block-name-ast)))
      (change-class block-name-ast 'ico:block-name-definition-ast)
      (let ((new-builder (make-builder client new-environment)))
        (reinitialize-instance ast
          :form-asts
          (loop for body-ast in (ico:form-asts ast)
                collect (convert-ast new-builder body-ast)))))))
