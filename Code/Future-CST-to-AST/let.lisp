(cl:in-package #:sicl-expression-to-ast)

;;; FIXME: handle declarations.
(defmethod abp:finish-node
    ((builder builder)
     (kind t)
     (ast ico:progn-ast))
  (with-builder-components (builder client environment)
    (loop for binding-ast in (ico:variable-binding-asts ast)
          for cst = (ico:origin (ico:form-ast binding-ast))
          do (reinitialize-instance binding-ast
               :form-ast (convert client cst environment)))
    (let ((body-environment environment))
      (loop for binding-ast in (ico:variable-binding-asts ast)
            for variable-name-ast = (ico:variable-name-ast binding-ast)
            do (setf environment
                     (augment-environment-with-variable
                      client
                      variable-name-ast
                      '()
                      body-environment
                      environment)))
      (reinitialize-instance ast
        :form-asts
        (loop for body-ast in (ico:form-asts ast)
              for cst in (ico:origin body-ast)
              collect (convert client cst body-environment))))))
