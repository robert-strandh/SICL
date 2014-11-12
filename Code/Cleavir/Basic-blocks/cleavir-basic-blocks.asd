(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-basic-blocks
  :depends-on (:cleavir-utilities
	       :cleavir-hir)
  :components
  ((:file "packages")
   (:file "basic-blocks" :depends-on ("packages"))))
