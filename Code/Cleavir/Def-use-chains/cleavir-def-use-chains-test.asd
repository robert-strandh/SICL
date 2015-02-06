(cl:in-package #:asdf-user)

(defsystem :cleavir-def-use-chains-test
  :depends-on (:cleavir-def-use-chains
	       :cleavir-test-utilities)
  :components
  ((:file "test-packages")
   (:file "test-def-use-chains" :depends-on ("test-packages"))))
