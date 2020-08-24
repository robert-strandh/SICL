(cl:in-package #:asdf-user)

(defsystem #:sicl-ast-evaluator
  :depends-on (#:cleavir2-ast
               #:cleavir2-cst-to-ast
               #:concrete-syntax-tree
               #:eclector
               #:clostrum
               #:clostrum/virtual)
  :serial t
  :components
  ((:file "packages")
   (:file "client")
   (:file "environment")
   (:file "trucler-methods")
   (:file "run-time")
   (:file "lexical-environment")
   (:file "translate-ast")
   (:file "translate-code")
   (:file "eval")))
