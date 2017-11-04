(cl:in-package #:asdf-user)

(defsystem sicl-variant-extrinsic
  :depends-on (:cleavir-code-utilities
               :cleavir-generate-ast
               :cleavir-cst-to-ast
               :cleavir-ast-to-hir
               :cleavir-hir-transformations)
  :serial t
  :components
  ((:file "packages")))
