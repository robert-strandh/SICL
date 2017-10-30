(cl:in-package #:asdf-user)

(defsystem :cleavir-partial-inlining
  :depends-on (:cleavir-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "generic-functions")
   (:file "mapping")
   (:file "worklist-item")))
