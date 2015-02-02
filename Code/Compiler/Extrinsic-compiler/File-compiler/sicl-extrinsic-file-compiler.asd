(cl:in-package #:asdf-user)

(defsystem :sicl-extrinsic-file-compiler
  :depends-on (:sicl-reader-simple
	       :cleavir-generate-ast
	       :cleavir-ast-to-hir
	       :cleavir-hir-to-mir
	       :sicl-extrinsic-environment
	       :sicl-target-sicl
	       :sicl-os-gnu-linux
	       :sicl-x86-64)
  :serial t
  :components
  ((:file "packages")
   (:file "compile-file")
   (:file "introduce-immediate")))
