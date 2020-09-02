(cl:in-package #:asdf-user)

(defsystem :cleavir-environment
  :depends-on (:acclimation :cleavir-ctype :cleavir-attributes)
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
   (:file "declarations")
   (:file "type-information")
   (:file "default-info-methods")
   (:file "eval")))
