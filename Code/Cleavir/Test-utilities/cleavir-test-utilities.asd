(cl:in-package #:asdf-user)

(defsystem :cleavir-test-utilities
  :depends-on (:cleavir-utilities)
  :components
  ((:file "packages")
   (:file "test-utilities" :depends-on ("packages"))))
