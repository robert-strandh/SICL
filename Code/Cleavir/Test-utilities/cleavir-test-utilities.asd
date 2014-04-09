(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-test-utilities
  :depends-on (:cleavir-utilities)
  :components
  ((:file "packages")
   (:file "test-utilities" :depends-on ("packages"))))
