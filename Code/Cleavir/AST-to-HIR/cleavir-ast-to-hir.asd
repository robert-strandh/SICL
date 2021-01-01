(cl:in-package #:asdf-user)

;;;; Compile an abstract syntax tree into an instruction graph.
;;;;
;;;; The result of the compilation is a single value, namely the first
;;;; instruction of the instruction graph resulting from the
;;;; compilation of the entire AST.

(defsystem #:cleavir-ast-to-hir
  :depends-on (#:acclimation
               #:stealth-mixin
               #:cleavir-ast
               #:cleavir-hir
               #:cleavir-cst-to-ast
               #:cleavir-primop)
  :serial t
  :components
  ((:file "packages")
   (:file "context")
   (:file "conditions")
   (:file "condition-reporters-english")
   (:file "utilities")
   (:file "compile-ast")
   (:file "compile-general-purpose-asts")
   (:file "compile-multiple-value-related-asts")
   (:file "compile-fixnum-related-asts")
   (:file "compile-character-related-asts")
   (:file "compile-simple-float-related-asts")
   (:file "compile-cons-related-asts")
   (:file "compile-standard-object-related-asts")
   (:file "compile-array-related-asts")))
