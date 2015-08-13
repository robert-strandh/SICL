(cl:in-package #:asdf-user)

(defsystem :cleavir-hir-transformations
  :depends-on (:cleavir-hir :cleavir-meter)
  :serial t
  :components
  ((:file "packages")
   (:file "constant-load-time-value")
   (:file "traverse")
   (:file "compute-ownership")
   (:file "inline-calls")
   (:file "static-few-assignments")
   (:file "type-inference")
   (:file "eliminate-load-time-value-inputs")
   (:file "coalesce-load-time-values")
   (:file "eliminate-typeq")
   (:file "simplify-box-unbox")
   (:file "function-tree")
   (:file "segregate-lexicals")
   (:file "eliminate-superfluous-temporaries")
   (:file "hir-transformations")))
