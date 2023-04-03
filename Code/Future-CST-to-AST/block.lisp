(cl:in-package #:sicl-expression-to-ast)

;;; We may want to introduce a subclass of the BLOCK-NAME-AST that
;;; contains a list of ASTs that refer to this block name.
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
      (let ((new-builder (make-builder client new-environment)))
        (reinitialize-instance ast
          :form-asts
          (loop for body-ast in (ico:form-asts ast)
                collect (convert-ast new-builder body-ast)))))))
