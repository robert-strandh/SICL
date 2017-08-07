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
   (:file "environment-augmentation")
   (:file "environment-query")
   (:file "variables")
   (:file "generic-functions")
   (:file "process-progn")
   (:file "convert-sequence")
   (:file "convert-variable")
   (:file "convert")
   (:file "convert-code")
   (:file "convert-lambda-call")
   (:file "convert-constant")
   (:file "convert-special")
   (:file "convert-cst")
   (:file "cst-to-ast")))
