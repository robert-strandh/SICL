(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-hir-interpreter
  :depends-on (:cleavir-hir
	       :cleavir-basic-blocks)
  :serial t
  :components
  ((:file "packages")
   (:file "translate")))
