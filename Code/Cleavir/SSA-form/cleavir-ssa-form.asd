(cl:in-package #:asdf-user)

(defsystem :cleavir-ssa-form
  :depends-on (:cleavir-utilities
	       :cleavir-dominance)
  :serial t
  :components
  ((:file "packages")
   (:file "ssa-form")))

