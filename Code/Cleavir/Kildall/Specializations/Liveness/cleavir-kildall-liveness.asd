(cl:in-package #:asdf-user)

(defsystem :cleavir-kildall-liveness
  :depends-on (:cleavir-kildall :cleavir-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "liveness")
   (:file "extend")))
