(cl:in-package #:asdf-user)

(defsystem :cleavir-lexical
  :depends-on (:acclimation :trucler-reference)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "query-functions")
   (:file "query-methods")
   (:file "optimize-qualities")
   (:file "type-information")))
