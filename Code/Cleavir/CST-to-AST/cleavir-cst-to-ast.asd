(cl:in-package #:asdf-user)

(defsystem :cleavir-cst-to-ast
  :depends-on (:concrete-syntax-tree
               :concrete-syntax-tree-destructuring
               :cleavir-ast
	       :cleavir-ast-transformations
	       :cleavir-primop
	       :cleavir-environment
	       :cleavir-compilation-policy
               :acclimation)
  :serial t
  :components
  ((:file "packages")
   (:file "generic-functions")
   (:file "process-progn")
   (:file "convert-sequence")
   (:file "convert-constant")
   (:file "convert-special")))
