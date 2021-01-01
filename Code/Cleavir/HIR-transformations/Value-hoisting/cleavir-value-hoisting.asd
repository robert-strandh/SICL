(cl:in-package #:asdf-user)

(defsystem :cleavir-value-hoisting
  :depends-on (:acclimation
               :cleavir-hir
               :cleavir-hir-transformations)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "condition-reporters-english")
   (:file "generic-functions")
   (:file "hoist-values")
   (:file "make-load-form-using-client")
   (:file "constructor")
   (:file "similarity-keys")
   (:file "scan")
   (:file "hoist")))
