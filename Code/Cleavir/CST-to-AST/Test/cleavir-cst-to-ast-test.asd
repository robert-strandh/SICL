(cl:in-package #:asdf-user)

(defsystem #:cleavir-cst-to-ast-test
  :depends-on (#:cleavir-cst-to-ast)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")))
