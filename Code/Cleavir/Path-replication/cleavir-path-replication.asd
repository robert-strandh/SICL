(cl:in-package #:asdf-user)

(defsystem :cleavir-path-replication
  :depends-on (:cleavir-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "rewrite")))
