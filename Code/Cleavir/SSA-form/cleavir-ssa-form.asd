(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-ssa-form
  :depends-on (:cleavir-utilities
	       :cleavir-dominance)
  :components
  ((:file "packages")
   (:file "ssa-form" :depends-on ("packages"))))

