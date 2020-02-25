(cl:in-package #:asdf-user)

(defsystem #:sicl-ast
  :depends-on (#:cleavir2-ast)
  :serial t
  :components
  ((:file "packages")
   (:file "dynamic-environment-ast")
   (:file "stack-asts")))
