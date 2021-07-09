(cl:in-package #:asdf-user)

(defsystem #:cleavir-register-allocation
  :depends-on (#:cleavir-liveness
               #:cleavir-ir
               #:cleavir-hir
               #:cleavir-mir)
  :components
  ((:file "packages")
   (:file "compute-conflicts")
   (:file "graph-coloring")))
