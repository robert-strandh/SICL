(cl:in-package #:asdf-user)

(defsystem :cleavir-equivalent-lexical-locations
  :depends-on (:cleavir-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "equivalent-lexical-locations")))
