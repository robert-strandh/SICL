(cl:in-package #:asdf-user)

(defsystem :cleavir-ast
  :depends-on (:cleavir-io
               :cleavir-attributes
	       :cleavir-meter)
  :serial t
  :components
  ((:file "packages")
   (:file "general-purpose-asts")
   (:file "fixnum-related-asts")
   (:file "simple-float-related-asts")
   (:file "cons-related-asts")
   (:file "standard-object-related-asts")
   (:file "array-related-asts")
   (:file "scope-related-asts")
   (:file "graphviz-drawing")
   (:file "map-ast")))
