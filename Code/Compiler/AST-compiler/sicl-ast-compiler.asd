(cl:in-package #:asdf-user)

(defsystem #:sicl-ast-compiler
  :depends-on (#:sicl-ast-to-hir
               #:sicl-hir-transformations
               #:sicl-hir-evaluator
               #:sicl-hir-to-mir
               #:sicl-mir-to-lir
               #:sicl-code-generation
               #:cluster
               #:cluster-x86-instruction-database
               #:sicl-code-object)
  :serial t
  :components
  ((:file "hoist-load-time-value")
   (:file "process-constant-inputs")
   (:file "establish-call-sites")
   (:file "eliminate-fdefinition-asts")
   (:file "ast-compiler")
   (:file "tie-code-object")
   (:file "cst-eval")))
