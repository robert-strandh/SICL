(cl:in-package #:asdf-user)

(defsystem :sicl-extrinsic-file-compiler
  :depends-on (:eclector
               :cleavir-code-utilities
               :cleavir-generate-ast
               :cleavir-cst-to-ast
	       :cleavir-generate-ast
               :cleavir-hir
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
