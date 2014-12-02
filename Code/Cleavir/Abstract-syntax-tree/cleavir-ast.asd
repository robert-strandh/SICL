(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-ast
  :depends-on (:cleavir-io)
  :serial t
  :components
  ((:file "packages")
   (:file "general-purpose-asts")
   (:file "fixnum-related-asts")
   (:file "float")
   (:file "cons")
   (:file "standard-object")
   (:file "array")
   (:file "graphviz-drawing")))
