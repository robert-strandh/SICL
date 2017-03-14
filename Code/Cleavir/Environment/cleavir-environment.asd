(cl:in-package #:asdf-user)

(defsystem :cleavir-environment
  :depends-on (:acclimation)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "condition-reporters-english")
   (:file "query")
   (:file "augmentation-functions")
   (:file "default-augmentation-classes")
   (:file "compile-time")
   (:file "optimize-qualities")
   (:file "type-information")
   (:file "default-info-methods")
   (:file "eval")))
