(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-mir
  :serial t
  :components
  ((:file "packages")
   (:file "general")
   (:file "fixnum")
   (:file "integer")
   (:file "float")
   (:file "cons")
   (:file "standard-object")
   (:file "array")
   (:file "graphviz-drawing")
   (:file "lir")))
