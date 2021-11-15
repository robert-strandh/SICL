(cl:in-package #:asdf-user)

(defsystem #:sicl-ast-transformations
  :depends-on (#:cleavir-ast
               #:sicl-ast)
  :serial t
  :components
  ((:file "packages")))
