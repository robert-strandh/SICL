(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-extrinsic-hir-interpreter
  :depends-on (:sicl-environment
	       :sicl-simple-environment
	       :cleavir-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "interpreter")
   (:file "instructions")))
