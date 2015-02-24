(cl:in-package #:asdf-user)

(defsystem :cleavir-ast-to-hir-test
  :depends-on (:cleavir-generate-ast-test
	       :cleavir-ast-to-hir
	       :cleavir-hir-interpreter)
  :serial t
  :components
  ((:file "packages")
   (:file "ast-to-hir")))
