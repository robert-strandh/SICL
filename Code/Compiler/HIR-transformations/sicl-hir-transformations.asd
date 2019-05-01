(cl:in-package #:asdf-user)

(defsystem sicl-hir-transformations
  :depends-on (#:cleavir2-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "hoist-fdefinitions")
   (:file "eliminate-create-cell")))
