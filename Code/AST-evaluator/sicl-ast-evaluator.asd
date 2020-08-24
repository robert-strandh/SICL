(cl:in-package #:asdf-user)

(defsystem #:sicl-ast-evaluator
  :depends-on (#:cleavir2-ast
               #:cleavir2-cst-to-ast
               #:cleavir-code-utilities
               #:concrete-syntax-tree
               #:eclector
               #:clostrum
               #:clostrum/virtual
               #:sicl-source-tracking)
  :serial t
  :components
  ((:file "packages")
   (:file "client")
   (:file "fill-environment")
   (:file "environment")
   (:file "trucler-methods")
   (:file "run-time")
   (:file "lexical-environment")
   (:file "translate-ast")
   (:file "translate-code")
   (:file "eval")))
