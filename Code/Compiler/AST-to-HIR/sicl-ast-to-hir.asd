(cl:in-package #:asdf-user)

(defsystem #:sicl-ast-to-hir
  :depends-on (#:cleavir2-cst-to-ast
               #:cleavir-ast-to-hir
               #:cleavir2-hir-transformations
               #:cleavir2-remove-useless-instructions
               #:cleavir2-partial-inlining
               #:sicl-hir-transformations
               #:sicl-argument-processing)
  :serial t
  :components
  ((:file "packages")
   (:file "client")
   (:file "ast-to-hir")
   (:file "customization")
   (:file "dynamic-environment")
   (:file "stack")))
