(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-register-allocation
  :depends-on (:cleavir-liveness
	       :cleavir-ir
	       :cleavir-hir
	       :cleavir-mir)
  :components
  ((:file "packages")
   (:file "compute-conflicts")
   (:file "graph-coloring")))
