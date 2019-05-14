(cl:in-package #:asdf-user)

(defsystem #:sicl-hir-to-cl-test
  :depends-on (#:sicl-alternative-extrinsic-environment
               #:concrete-syntax-tree
               #:cleavir2-ast
               #:cleavir2-cst-to-ast
               #:sicl-ast-to-hir
               #:sicl-hir-to-cl)
  :components
  ((:file "packages")))
