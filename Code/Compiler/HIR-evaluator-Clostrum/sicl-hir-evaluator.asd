(cl:in-package #:asdf-user)

(defsystem #:sicl-hir-evaluator-clostrum
  :depends-on (#:closer-mop
               #:cleavir2-hir
               #:cleavir2-cst-to-ast
               #:sicl-utilities
               #:sicl-ast-to-hir
               #:sicl-hir-transformations
               #:sicl-environment
               #:sicl-data-and-control-flow-support)
  :serial t
  :components
  ((:file "packages")
   (:file "lexical-environment")
   (:file "evaluator")
   (:file "make-thunk")
   (:file "run-time")
   (:file "general-purpose-instructions")
   (:file "initialize-closure-instruction")
   (:file "argument-processing-instructions")
   (:file "return-value-related-instructions")
   (:file "array-related-instructions")
   (:file "boxing-related-instructions")
   (:file "cons-related-instructions")
   (:file "character-related-instructions")
   (:file "fixnum-related-instructions")
   (:file "multiple-value-related-instructions")
   (:file "catch-instruction")
   (:file "unwind-instruction")
   (:file "bind-instruction")
   (:file "enclose-instruction")
   (:file "breakpoint-instruction")
   (:file "cst-eval")))
