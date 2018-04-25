(cl:in-package #:asdf-user)

(defsystem :cleavir-hir-transformations
  :depends-on (:cleavir-hir :cleavir-meter)
  :serial t
  :components
  ((:file "packages")
   (:file "constant-load-time-value")
   (:file "traverse")
   (:file "convert-constant-to-immediate")
   (:file "hoist-load-time-values")
   (:file "compute-ownership")
   (:file "static-few-assignments")
   (:file "type-inference")
   (:file "eliminate-load-time-value-inputs")
   (:file "coalesce-load-time-values")
   (:file "eliminate-typeq")
   (:file "simplify-box-unbox")
   (:file "function-dag")
   (:file "segregate-lexicals")
   (:file "escape")
   (:file "eliminate-superfluous-temporaries")
   (:file "hir-transformations")))
