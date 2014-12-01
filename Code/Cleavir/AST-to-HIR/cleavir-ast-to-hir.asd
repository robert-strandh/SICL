(cl:in-package #:common-lisp-user)

;;;; Compile an abstract syntax tree into an instruction graph.
;;;;
;;;; The result of the compilation is a single value, namely the first
;;;; instruction of the instruction graph resulting from the
;;;; compilation of the entire AST.

(asdf:defsystem :cleavir-ast-to-hir
  :depends-on (:cleavir-ast :cleavir-hir :cleavir-primop)
  :serial t
  :components
  ((:file "packages")
   (:file "context")
   (:file "compile-general-purpose-asts")
   (:file "compile-fixnum-related-asts")
   (:file "float")
   (:file "cons")
   (:file "standard-object")
   (:file "compile-array-related-asts")))
