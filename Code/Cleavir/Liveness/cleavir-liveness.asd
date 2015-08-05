(cl:in-package #:asdf-user)

(defsystem :cleavir-liveness
  :depends-on (:cleavir-utilities :cleavir-hir)
  :components
  ((:file "packages")
   (:file "liveness" :depends-on ("packages"))))
