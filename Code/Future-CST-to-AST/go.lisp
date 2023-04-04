(cl:in-package #:sicl-expression-to-ast)

(defmethod abp:finish-node
    ((builder builder)
     (kind t)
     (ast ico:go-ast))
  (with-builder-components (builder client environment)
    (let* ((referencing-tag-ast (ico:tag-ast ast))
           (name (ico:name referencing-tag-ast))
           (description (describe-tag client environment name))
           (defining-tag-ast (trucler:identity description)))
      (reinitialize-instance referencing-tag-ast
        :tag-definition-ast defining-tag-ast)
      (reinitialize-instance defining-tag-ast
        :tag-reference-asts
        (append (ico:tag-reference-asts defining-tag-ast)
                (list referencing-tag-ast))))))
