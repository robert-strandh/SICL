(cl:in-package #:asdf-user)

(defsystem #:sicl-hir-interpreter-test
  :depends-on (#:sicl-extrinsic-environment
               #:concrete-syntax-tree
               #:cleavir2-ast
               #:cleavir2-cst-to-ast
               #:cleavir2-primop
               #:sicl-hir-interpreter)
  :components
  ((:file "packages")
   (:file "eval")
   (:file "test")))
