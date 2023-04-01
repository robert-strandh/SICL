(cl:in-package #:sicl-expression-to-ast)

(defgeneric eval-cooked (client cooked-expression environment))
