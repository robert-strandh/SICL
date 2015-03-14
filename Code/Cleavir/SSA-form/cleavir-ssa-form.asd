(cl:in-package #:asdf-user)

(defsystem :cleavir-ssa-form
  :depends-on (:cleavir-utilities
	       :cleavir-dominance)
  :components
  ((:file "packages")
   (:file "ssa-form" :depends-on ("packages"))))

