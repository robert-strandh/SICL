(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-compiler
  :depends-on (:sicl-code-utilities
	       :sicl-environment
	       :sicl-reader-simple
	       :sicl-generate-ast
	       :sicl-type
	       :cleavir-ast
	       :cleavir-primop)
  :serial t
  :components
  ((:file "packages")
   (:file "ast-transformations")
   (:file "graph-coloring")
   (:file "make")
   (:file "program")
   (:file "type-map")
   (:file "type-inference")
   (:file "compile-file")
   (:file "compile-lambda-expression")
   (:file "compile-time-compile")))
