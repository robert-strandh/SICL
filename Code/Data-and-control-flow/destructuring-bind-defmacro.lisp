(cl:in-package #:sicl-data-and-control-flow)

(defmacro destructuring-bind (lambda-list form &body body)
  (cleavir-code-utilities:parse-destructuring-bind lambda-list form body))
