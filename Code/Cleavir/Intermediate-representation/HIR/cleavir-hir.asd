(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-hir
  :depends-on (:cleavir-ir)
  :serial t
  :components
  ((:file "general")
   (:file "fixnum")
   (:file "integer")
   (:file "float")
   (:file "cons")
   (:file "standard-object")
   (:file "array")
   (:file "graphviz-drawing")))
