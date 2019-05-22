(cl:in-package #:cleavir-cst-to-ast)

(defgeneric cst-eval (client cst environment))

(defgeneric eval (client form environment))

(defmethod eval (client form environment)
  (cst-eval client (cst:cst-from-expression form) environment))
