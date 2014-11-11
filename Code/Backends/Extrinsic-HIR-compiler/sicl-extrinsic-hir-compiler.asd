(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-extrinsic-hir-compiler
  :depends-on (:sicl-environment
	       :sicl-simple-environment
	       :cleavir-hir
	       :sicl-extrinsic-environment
	       :cleavir-hir-transformations)
  :serial t
  :components
  ((:file "packages")
   (:file "define-global-environment")
   (:file "create-global-environment")
   (:file "error")
   (:file "primitive-function")
   (:file "fill-global-environment")
   (:file "runtime-environment")
   (:file "parse-arguments")
   (:file "translate-hir")))
