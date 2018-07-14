(cl:in-package #:asdf-user)

(defsystem #:cleavir-load-time-value-hoisting-test
  :depends-on (#:cleavir-load-time-value-hoisting
               #:sicl-extrinsic-environment)

  :serial t
  :components
  ((:file "packages")
   (:file "test-system")
   (:file "test")))
