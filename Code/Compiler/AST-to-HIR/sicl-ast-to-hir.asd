(cl:in-package #:asdf-user)

(defsystem #:sicl-ast-to-hir
  :depends-on (#:cleavir2-cst-to-ast
               #:cleavir2-ast-to-hir
               #:cleavir2-hir-transformations
               #:sicl-hir-transformations
               #:sicl-argument-processing)
  :serial t
  :components
  ((:file "packages")
   (:file "ast-to-hir")))
