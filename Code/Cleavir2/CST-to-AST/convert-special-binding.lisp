(cl:in-package #:cleavir-cst-to-ast)

(defmethod convert-special-binding (client
                                    variable-cst
                                    value-ast
                                    body-function
                                    lexical-environment
                                    dynamic-environment-ast)
  (let ((dynamic-environment-output-ast
          (make-instance 'cleavir-ast:lexical-ast
            :name '#:bound-env
            :dynamic-environment-input-ast dynamic-environment-ast)))
    (make-instance 'cleavir-ast:bind-ast
      :name-ast (make-instance 'cleavir-ast:constant-ast
                  :value (cst:raw variable-cst)
                  :dynamic-environment-input-ast dynamic-environment-ast)
      :value-ast value-ast
      :body-ast (funcall body-function dynamic-environment-output-ast)
      :dynamic-environment-input-ast dynamic-environment-ast
      :dynamic-environment-output-ast dynamic-environment-output-ast)))
