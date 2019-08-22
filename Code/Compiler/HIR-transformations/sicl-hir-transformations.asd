(cl:in-package #:asdf-user)

(defsystem sicl-hir-transformations
  :depends-on (#:cleavir2-hir
               #:cleavir2-ast-to-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "convert-symbol-value")
   (:file "hoist-fdefinitions")
   (:file "eliminate-create-cell")
   (:file "eliminate-fetch")
   (:file "eliminate-cell-access")))
