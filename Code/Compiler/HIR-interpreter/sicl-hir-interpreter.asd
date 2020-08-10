(cl:in-package #:asdf-user)

(defsystem #:sicl-hir-interpreter
  :depends-on (#:closer-mop
               #:cleavir2-hir
               #:cleavir2-cst-to-ast
               #:sicl-ast-to-hir
               #:sicl-hir-transformations
               #:sicl-global-environment)
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "call-stack")
   (:file "dynamic-environment")
   (:file "run-time")
   (:file "general-purpose-instructions")
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
   (:file "hir-interpreter")
   (:file "enclose-instruction")
   (:file "initialize-closure-instruction")
   (:file "breakpoint-instruction")
   (:file "cst-eval")))
