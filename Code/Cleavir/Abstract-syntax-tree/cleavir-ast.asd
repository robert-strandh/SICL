(cl:in-package #:asdf-user)

(defsystem :cleavir-ast
  :depends-on (:cleavir-io)
  :serial t
  :components
  ((:file "packages")
   (:file "general-purpose-asts")
   (:file "fixnum-related-asts")
   (:file "float-related-asts")
   (:file "cons-related-asts")
   (:file "standard-object-related-asts")
   (:file "array-related-asts")
   (:file "graphviz-drawing")))
