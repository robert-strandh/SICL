(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-register-allocation
  :components
  ((:file "packages")
   (:file "graph-coloring")))
