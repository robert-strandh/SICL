(cl:in-package #:sicl-expression-to-ast)

(defgeneric convert (client cooked-expression environment))

(defgeneric convert-with-description
    (client cooked-expression info environment))
