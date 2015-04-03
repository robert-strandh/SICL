(cl:in-package #:asdf-user)

(defsystem :cleavir-hir-interpreter
  :depends-on (:cleavir-hir
	       :cleavir-hir-transformations
	       :cleavir-basic-blocks)
  :serial t
  :components
  ((:file "packages")
   (:file "translate")))
