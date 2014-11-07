(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-extrinsic-hir-compiler
  :depends-on (:sicl-environment
	       :sicl-simple-environment
	       :cleavir-hir
	       :sicl-extrinsic-environment)
  :serial t
  :components
  ((:file "packages")
   (:file "define-global-environment")
   (:file "create-global-environment")
   (:file "fill-global-environment")
   (:file "runtime-environment")))

