(cl:in-package #:asdf-user)

(defsystem #:cleavir-load-time-value-hoisting-test
  :depends-on (#:cleavir-generate-ast
               #:cleavir-ast-to-hir
               #:cleavir-load-time-value-hoisting
               #:sicl-extrinsic-environment)

  :serial t
  :components
  ((:file "packages")
   (:file "client")
   (:file "test")))
