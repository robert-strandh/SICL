(cl:in-package #:asdf-user)

(defsystem #:sicl-cst-to-ast
  :depends-on (#:sicl-primop
               #:sicl-ast
               #:sicl-client
               #:cleavir-cst-to-ast)
  :serial t
  :components
  ((:file "packages")
   (:file "cleavir-configuration")
   (:file "symbol-value")
   (:file "declaration-proclamations")
   (:file "dynamic-environment")
   (:file "stack")
   (:file "convert-standard-object-primops")
   (:file "cst-to-ast")))
