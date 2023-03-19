(cl:in-package #:sicl-future-cst-to-ast)

(defgeneric convert (client cst environment))

(defgeneric convert-cst (client cst info environment))
