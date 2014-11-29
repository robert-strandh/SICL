(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-extrinsic-hir-compiler
  :depends-on (:sicl-environment
	       :sicl-simple-environment
	       :sicl-reader-simple
	       :cleavir-hir
	       :cleavir-ast-to-hir
	       :sicl-extrinsic-environment
	       :cleavir-hir-transformations
	       :cleavir-basic-blocks)
  :serial t
  :components
  ((:file "packages")
   (:file "define-global-environment")
   (:file "create-global-environment")
   (:file "runtime-environment")
   (:file "error")
   (:file "primitive-function")
   (:file "fill-global-environment")
   (:file "parse-arguments")
   (:file "translate-hir")
   (:file "eval")
   (:file "load")))
