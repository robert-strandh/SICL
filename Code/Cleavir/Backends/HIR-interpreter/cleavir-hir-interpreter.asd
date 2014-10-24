(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-hir-interpreter
  :depends-on (:cleavir-hir
	       :cleavir-lexical-depth)
  :serial t
  :components
  ((:file "packages")
   (:file "translate")))
