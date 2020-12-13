(cl:in-package #:asdf-user)

(defsystem #:sicl-cst-to-ast
  :depends-on (#:sicl-primop
               #:sicl-ast
               #:sicl-client
               #:cleavir2-cst-to-ast)
  :serial t
  :components
  ((:file "packages")
   (:file "symbol-value")
   (:file "dynamic-environment")
   (:file "stack")))
