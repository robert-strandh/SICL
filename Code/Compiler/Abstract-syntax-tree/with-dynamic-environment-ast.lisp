(cl:in-package #:sicl-ast)

(defclass with-dynamic-environment-ast (cleavir-ast:ast)
  ((%dynamic-environment-ast :initarg :dynamic-environment-ast
                             :reader dynamic-environment-ast)
   (%body-ast :initarg :body-ast :reader body-ast)))
