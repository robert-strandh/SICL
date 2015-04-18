(cl:in-package #:asdf-user)

(defsystem :cleavir-basic-blocks
  :depends-on (:cleavir-utilities
	       :cleavir-hir)
  :components
  ((:file "packages")
   (:file "basic-blocks" :depends-on ("packages"))))
