(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-base
  :depends-on (#:cleavir-code-utilities
               #:sicl-client
               #:sicl-environment
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
               #:sicl-symbol-support
               #:sicl-string-support
               #:sicl-character-support
               #:sicl-type-support
               #:sicl-stream-support
               #:sicl-ast-evaluator
               #:sicl-cst-to-ast
               #:sicl-allocator
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
   (:file "asdf-programming")
   (:file "create-accessor-generic-functions")
   (:file "create-mop-classes")))
