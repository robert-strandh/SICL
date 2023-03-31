(cl:in-package #:sicl-expression-to-ast)

(defgeneric convert (client cst environment))

(defgeneric convert-with-description (client cst info environment))
