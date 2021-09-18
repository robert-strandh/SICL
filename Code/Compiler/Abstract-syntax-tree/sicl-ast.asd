(cl:in-package #:asdf-user)

(defsystem #:sicl-ast
  :depends-on (#:cleavir-ast)
  :serial t
  :components
  ((:file "packages")
   (:file "dynamic-environment-ast")
   (:file "stack-asts")
   (:file "with-dynamic-environment-ast")
   (:file "standard-object-asts")
   (:file "patch-literal-ast")))
