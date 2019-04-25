(cl:in-package #:asdf-user)

(defsystem :cleavir2-ast
  :depends-on (:cleavir-io
	       :cleavir-meter)
  :serial t
  :components
  ((:file "packages")
   (:file "ast")
   (:file "general-purpose-asts")
   (:file "fixnum-related-asts")
   (:file "simple-float-related-asts")
   (:file "cons-related-asts")
   (:file "standard-object-related-asts")
   (:file "array-related-asts")
   (:file "scope-related-asts")
   (:file "map-ast")))
