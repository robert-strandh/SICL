(cl:in-package #:cleavir-cst-to-ast)

(defmethod convert-special-binding (client
                                    variable-cst
                                    value-ast
                                    body-function
                                    lexical-environment
                                    dynamic-environment-ast)
  (let ((dynamic-environment-output-ast
          (make-instance 'cleavir-ast:lexical-ast :name '#:bound-env)))
    (make-instance 'cleavir-ast:bind-ast
      :name-ast (convert client
                 variable-cst
                 lexical-environment
                 dynamic-environment-ast)
      :value-ast value-ast
      :body-ast (funcall body-function dynamic-environment-output-ast)
      :dynamic-environment-output-ast dynamic-environment-output-ast)))
