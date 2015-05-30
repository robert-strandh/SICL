(cl:in-package #:asdf-user)

(defsystem :cleavir-hir-to-mir
  :depends-on (:cleavir-hir
	       :cleavir-mir)
  :serial t
  :components
  ((:file "general")))
