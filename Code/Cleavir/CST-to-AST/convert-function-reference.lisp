(cl:in-package #:cleavir-cst-to-ast)

(defmethod convert-function-reference
    ((info cleavir-env:global-function-info) env system)
  (convert-global-function-reference
   info (cleavir-env:global-environment env) system))

(defmethod convert-function-reference
    ((info cleavir-env:local-function-info) env system)
  (declare (ignore env system))
  (cleavir-env:identity info))

(defmethod convert-function-reference
    ((info cleavir-env:global-macro-info) env system)
  (error 'function-name-names-global-macro
         :expr (cleavir-env:name info)))

(defmethod convert-function-reference
    ((info cleavir-env:local-macro-info) env system)
  (error 'function-name-names-local-macro
         :expr (cleavir-env:name info)))

(defmethod convert-function-reference
    ((info cleavir-env:special-operator-info) env system)
  (error 'function-name-names-special-operator
         :expr (cleavir-env:name info)))
