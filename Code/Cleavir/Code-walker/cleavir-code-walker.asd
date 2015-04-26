(cl:in-package #:asdf-user)

(defsystem :cleavir-code-walker
  :depends-on (:cleavir-code-utilities
	       :cleavir-environment)
  :serial t
  :components
  ((:file "packages")
   (:file "walk-form")))
