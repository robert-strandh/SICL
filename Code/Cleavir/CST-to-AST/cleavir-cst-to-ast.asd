(cl:in-package #:asdf-user)

(defsystem :cleavir-cst-to-ast
  :depends-on (:concrete-syntax-tree
               :cleavir-ast
	       :cleavir-ast-transformations
	       :cleavir-primop
	       :cleavir-environment
	       :cleavir-compilation-policy
               :acclimation)
  :serial t
  :components
  ((:file "packages")))
