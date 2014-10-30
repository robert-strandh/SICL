(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-mir
  :serial t
  :components
  ((:file "general")
   (:file "graphviz-drawing")))
