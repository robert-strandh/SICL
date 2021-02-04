(cl:in-package #:asdf-user)

(defsystem #:sicl-ast-to-hir
  :depends-on (#:cleavir-cst-to-ast
               #:cleavir-ast-to-hir
               #:cleavir-hir-transformations
               #:cleavir-remove-useless-instructions
               #:cleavir-partial-inlining
               #:sicl-hir-transformations
               #:sicl-argument-processing)
  :serial t
  :components
  ((:file "packages")
   (:file "client")
   (:file "customization")
   (:file "dynamic-environment")
   (:file "stack")))
