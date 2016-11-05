(cl:in-package #:asdf-user)

(defsystem cleavir-typed-transforms
  :depends-on (:cleavir-type-inference
	       :cleavir-hir
	       :cleavir-compilation-policy)
  :serial t
  :components
  ((:file "packages")
   (:file "independent")
   (:file "dependent")))
