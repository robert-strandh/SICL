(cl:in-package #:asdf-user)

(defsystem #:sicl-ast-to-hir
  :depends-on (#:cleavir2-cst-to-ast
               #:cleavir2-ast-to-hir
               #:cleavir2-hir-transformations
               #:cleavir2-remove-useless-instructions
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
