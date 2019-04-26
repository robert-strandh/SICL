(cl:in-package #:asdf-user)

(defsystem :cleavir2-hir-transformations
  :depends-on (:cleavir2-hir :cleavir-meter)
  :serial t
  :components
  ((:file "packages")
   (:file "compute-ownership")
   (:file "function-dag")
   (:file "segregate-lexicals")))
