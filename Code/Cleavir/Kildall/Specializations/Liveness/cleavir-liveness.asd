(cl:in-package #:asdf-user)

(defsystem :cleavir-liveness
  :depends-on (:cleavir-kildall :cleavir-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "sset")
   (:file "liveness")
   (:file "extend")))
