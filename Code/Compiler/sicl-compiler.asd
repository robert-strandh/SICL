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
   (:file "compile-file")))
