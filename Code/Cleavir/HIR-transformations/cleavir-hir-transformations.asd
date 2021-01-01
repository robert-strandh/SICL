(cl:in-package #:asdf-user)

(defsystem :cleavir-hir-transformations
  :depends-on (:cleavir-hir :cleavir-meter)
  :serial t
  :components
  ((:file "packages")
   (:file "eliminate-catches")
   (:file "compute-ownership")
   (:file "function-dag")
   (:file "escape")
   (:file "segregate-lexicals")
   (:file "replace-aliases")))
