(cl:in-package #:asdf-user)

(defsystem #:cleavir-dominance-test
  :depends-on (#:cleavir-utilities
               #:cleavir-dominance
               #:cleavir-test-utilities)
  :serial t
  :components
  ((:file "test-packages")
   (:file "test-dominance")))
