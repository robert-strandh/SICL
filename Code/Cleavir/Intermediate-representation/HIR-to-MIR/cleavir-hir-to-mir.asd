(cl:in-package #:common-lisp-user)

(asdf:defsystem #:cleavir-hir-to-mir
  :depends-on (:cleavir-hir
	       :cleavir-mir)
  :serial t
  :components
  ((:file "general")))
