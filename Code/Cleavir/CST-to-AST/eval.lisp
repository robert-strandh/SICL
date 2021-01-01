(cl:in-package #:cleavir-cst-to-ast)

(defgeneric cst-eval (client cst environment))

(defmethod cst-eval :around (client cst environment)
  (with-encapsulated-conditions
      (cst eval-error eval-warning eval-style-warning)
    (call-next-method)))

(defgeneric eval (client form environment))

(defmethod eval (client form environment)
  (cst-eval client (cst:cst-from-expression form) environment))
