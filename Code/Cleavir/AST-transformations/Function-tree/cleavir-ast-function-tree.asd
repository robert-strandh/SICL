(cl:in-package #:asdf-user)

(defsystem #:cleavir-ast-function-tree
  :depends-on (#:cleavir-ast)
  :serial t
  :components
  ((:file "packages")
   (:file "function-tree")))
