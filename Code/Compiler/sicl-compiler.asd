(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-compiler
  :depends-on (:cleavir-code-utilities
	       :sicl-global-environment
	       :sicl-reader-simple
	       :sicl-generate-ast
	       :sicl-type
	       :cleavir-ast
	       :cleavir-primop)
  :serial t
  :components
  ((:file "packages")
   (:file "compile-file")))
