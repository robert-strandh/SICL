(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-mir
  :depends-on (:cleavir-ir)
  :serial t
  :components
  ((:file "general")
   (:file "graphviz-drawing")))
