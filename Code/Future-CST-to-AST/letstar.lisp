(cl:in-package #:sicl-expression-to-ast)

;;; FIXME: handle more declarations
(defmethod abp:finish-node
    ((builder builder)
     (kind t)
     (ast ico:let*-ast))
  (with-accessors ((binding-asts ico:variable-binding-asts)
                   (declaration-asts ico:declaration-asts))
      ast
    (with-builder-components (builder client environment)
      (let ((new-environment environment)
            (new-builder builder))
        (loop for binding-ast in binding-asts
              for form-ast = (ico:form-ast binding-ast)
              for variable-name-ast = (ico:variable-name-ast binding-ast)
              for variable-name = (ico:name variable-name-ast)
              do (reinitialize-instance binding-ast
                   :form-ast (convert-ast new-builder form-ast))
                 (multiple-value-bind (special-p globally-special-p)
                     (variable-is-special-p
                      client
                      new-environment
                      variable-name-ast
                      declaration-asts)
                   (change-class variable-name-ast
                                 (if special-p
                                     'ico:special-variable-bound-ast
                                     'ico:variable-definition-ast))
                   (setf new-environment
                         (if special-p
                             (unless globally-special-p
                               (trucler:add-local-special-variable
                                client new-environment variable-name))
                             (trucler:add-lexical-variable
                              client new-environment
                              variable-name variable-name-ast))))
                 (setf new-builder
                       (make-builder client new-environment)))
        (reinitialize-instance ast
          :form-asts
          (loop for body-ast in (ico:form-asts ast)
                collect (convert-ast new-builder body-ast)))))))
