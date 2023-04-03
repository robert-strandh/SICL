(cl:in-package #:sicl-expression-to-ast)

(defmethod abp:finish-node
    ((builder builder)
     (kind t)
     (ast ico:return-from-ast))
  (with-builder-components (builder client environment)
    (let* ((block-name-ast (ico:name-ast ast))
           (name (ico:name block-name-ast))
           (block-description (describe-block client environment name))
           (identity (trucler:identity block-description)))
      (change-class block-name-ast 'ico:block-name-reference-ast
                    :block-name-definition-ast identity)
      (reinitialize-instance identity
        :block-name-reference-asts
        (append (ico:block-name-reference-asts identity)
                (list block-name-ast)))
      (reinitialize-instance ast
        :form-asts
        (loop for body-ast in (ico:form-asts ast)
              collect (convert-ast builder body-ast))))))
