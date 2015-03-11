(cl:in-package #:asdf-user)

(defsystem :cleavir-dominance
  :depends-on (:cleavir-utilities
	       :cleavir-meter)
  :components
  ((:file "packages" :depends-on ())
   (:file "dominance" :depends-on ("packages"))))
