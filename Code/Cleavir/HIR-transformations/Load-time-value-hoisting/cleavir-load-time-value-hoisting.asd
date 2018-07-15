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
   (:file "generic-functions")
   (:file "constructor")
   (:file "make-constructor")
   (:file "immediate-p")
   (:file "equalp-keys")
   (:file "scan")
   (:file "hoist")
   (:file "load-time-value-hoisting")))
