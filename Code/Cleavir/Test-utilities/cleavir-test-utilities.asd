(cl:in-package #:asdf-user)

(defsystem :cleavir-test-utilities
  :depends-on (:cleavir-utilities)
  :serial t
  :components
  ((:file "packages")
   (:file "test-utilities")))
