(cl:in-package #:cleavir-cst-to-ast)

(defmethod convert-special-binding
    (client variable-cst value-ast body-function environment)
  (cleavir-ast:make-ast 'cleavir-ast:bind-ast
    :name-ast (cleavir-ast:make-ast 'cleavir-ast:literal-ast
                :value (cst:raw variable-cst))
    :value-ast value-ast
    :body-ast (funcall body-function)))
