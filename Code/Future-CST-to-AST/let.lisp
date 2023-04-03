(cl:in-package #:sicl-expression-to-ast)

;;; FIXME: handle declarations.
(defmethod abp:finish-node
    ((builder builder)
     (kind t)
     (ast ico:let-ast))
  (with-builder-components (builder client environment)
    ;; Start by converting the initialization arguments of
    ;; each binding, using the original builder.
    (loop for binding-ast in (ico:variable-binding-asts ast)
          for form-ast = (ico:form-ast binding-ast)
          do (reinitialize-instance binding-ast
               :form-ast (convert-ast builder form-ast)))
    (let ((body-environment environment))
      (loop for binding-ast in (ico:variable-binding-asts ast)
            for variable-name-ast = (ico:variable-name-ast binding-ast)
            do (change-class variable-name-ast
                             'ico:variable-definition-ast)
               (setf body-environment
                     (augment-environment-with-variable
                      client
                      variable-name-ast
                      '()
                      body-environment
                      environment)))
      (let ((new-builder (make-builder client body-environment)))
        (reinitialize-instance ast
          :form-asts
          (loop for body-ast in (ico:form-asts ast)
                collect (convert-ast new-builder body-ast)))))))
