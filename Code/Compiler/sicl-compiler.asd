(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-compiler
  :depends-on (:sicl-code-utilities
	       :sicl-compiler-utilities
	       :sicl-compiler-basic-blocks
	       :sicl-compiler-dominance
	       :sicl-compiler-liveness
	       :sicl-compiler-reaching-definitions
	       :sicl-compiler-def-use-chains
	       :sicl-compiler-ssa-form
	       :sicl-compiler-loops
	       :sicl-environment
	       :sicl-reader-simple
	       :sicl-type
	       :cleavir-ast
	       :cleavir-primop)
  :serial t
  :components
  ((:file "packages")
   (:file "abstract-syntax-tree")
   (:file "mir")
   (:file "new-phase1")
   (:file "ast-transformations")
   (:file "phase2")
   (:file "graph-coloring")
   (:file "make")
   (:file "program")
   (:file "type-map")
   (:file "type-inference")
   (:file "compile-file")
   (:file "compile-lambda-expression")
   (:file "compile-time-compile")))
