(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-mir
  :serial t
  :components
  ((:file "packages")
   (:file "general")
   (:file "fixnum")
   (:file "float")
   (:file "accessors")
   (:file "reduce")
   (:file "graphviz-drawing")))
