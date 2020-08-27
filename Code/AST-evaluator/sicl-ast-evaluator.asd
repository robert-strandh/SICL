(cl:in-package #:asdf-user)

(defsystem #:sicl-ast-evaluator
  :depends-on (#:cleavir2-ast
               #:cleavir2-cst-to-ast
               #:cleavir-code-utilities
               #:concrete-syntax-tree
               #:eclector
               #:sicl-client
               #:sicl-environment
               #:sicl-source-tracking)
  :serial t
  :components
  ((:file "packages")
   (:file "client")
   (:file "fill-environment")
   (:file "environment")
   (:file "run-time")
   (:file "lexical-environment")
   (:file "translate-ast")
   (:file "translate-character-related-asts")
   (:file "translate-array-related-asts")
   (:file "translate-cons-related-asts")
   (:file "translate-fixnum-related-asts")
   (:file "translate-multiple-value-related-asts")
   (:file "translate-code")
   (:file "eval")))
