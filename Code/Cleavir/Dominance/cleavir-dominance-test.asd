(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-dominance-test
  :depends-on (:cleavir-utilities
	       :cleavir-dominance
	       :cleavir-test-utilities)
  :components
  ((:file "test-packages" :depends-on ())
   (:file "test-dominance" :depends-on ("test-packages"))))
