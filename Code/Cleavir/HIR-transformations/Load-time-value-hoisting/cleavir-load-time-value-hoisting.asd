(cl:in-package #:asdf-user)

(defsystem :cleavir-load-time-value-hoisting
  :depends-on (:acclimation
               :cleavir-hir
               :cleavir-hir-transformations)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "condition-reporters-english")
   (:file "utilities")
   (:file "constructors")
   (:file "scan")
   (:file "reconstruct")
   (:file "load-time-value-hoisting")))
