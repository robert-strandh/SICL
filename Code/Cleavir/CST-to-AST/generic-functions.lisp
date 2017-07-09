(cl:in-package #:cleavir-cst-to-ast)

(defgeneric convert-cst (cst info environment system))

(defgeneric convert-special (head cst environment system))
