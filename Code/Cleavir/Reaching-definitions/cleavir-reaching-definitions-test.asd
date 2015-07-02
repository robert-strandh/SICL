(cl:in-package #:asdf-user)

(defsystem :cleavir-reaching-definitions-test
  :depends-on (:cleavir-utilities
	       :cleavir-test-utilities
	       :cleavir-reaching-definitions)
  :components
  ((:file "test-packages")
   (:file "test-reaching-definitions" :depends-on ("test-packages"))))

