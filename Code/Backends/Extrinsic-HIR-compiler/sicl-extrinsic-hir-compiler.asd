(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-extrinsic-hir-compiler
  :depends-on (:sicl-environment
	       :sicl-simple-environment
	       :cleavir-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "global-environment")
   (:file "runtime-environment")))

