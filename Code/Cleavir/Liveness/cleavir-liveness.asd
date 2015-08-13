(cl:in-package #:asdf-user)

(defsystem :cleavir-liveness
  :depends-on (:cleavir-utilities
	       :cleavir-hir
	       :cleavir-meter)
  :components
  ((:file "packages")
   (:file "liveness" :depends-on ("packages"))))
