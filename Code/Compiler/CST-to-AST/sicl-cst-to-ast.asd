(cl:in-package #:asdf-user)

(defsystem #:sicl-cst-to-ast
  :depends-on (#:sicl-primop
               #:sicl-ast
               #:cleavir2-cst-to-ast)
  :serial t
  :components
  ((:file "packages")
   (:file "dynamic-environment")
   (:file "stack")))
