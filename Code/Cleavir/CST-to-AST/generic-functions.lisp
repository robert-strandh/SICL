(cl:in-package #:cleavir-cst-to-ast)

(defgeneric convert (cst environment system))

(defgeneric convert-cst (cst info environment system))

(defgeneric convert-special (head cst environment system))
