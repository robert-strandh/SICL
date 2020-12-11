(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-base
  :depends-on (#:cleavir-code-utilities
               #:sicl-environment
               #:sicl-client
               #:sicl-ast-to-hir
               #:sicl-hir-evaluator
               #:sicl-hir-to-mir
               #:sicl-mir-to-lir
               #:sicl-source-tracking
               #:sicl-code-generation
               #:sicl-code-object
               #:sicl-clos-support
               #:sicl-method-combination-support
               #:sicl-array-support
               #:sicl-package-support
               #:sicl-string-support
               #:sicl-character-support
               #:sicl-type-support
               #:sicl-printer-support
               #:sicl-stream-support
               #:sicl-ast-evaluator
               #:eclector
               #:eclector-concrete-syntax-tree
               #:cleavir-io
               #:sicl-boot-backtrace-inspector
               #:clouseau)
  :serial t
  :components
  ((:file "packages")
   (:file "client")
   (:file "environment")
   (:file "boot-class")
   (:file "header")
   (:file "utilities")
   (:file "create-accessor-generic-functions")
   (:file "create-mop-classes")))
