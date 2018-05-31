(cl:in-package #:asdf-user)

(defsystem :sicl-extrinsic-file-compiler
  :depends-on (:eclector
	       :cleavir-generate-ast
	       :cleavir-ast-to-hir
	       :cleavir-ast-transformations
	       :cleavir-hir-to-mir
	       :sicl-extrinsic-environment
	       :sicl-target-sicl
	       :sicl-os-gnu-linux
	       :cleavir-processor-x86-64
	       :sicl-hir-to-mir)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "compile-file")))
