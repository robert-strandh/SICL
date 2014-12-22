(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-extrinsic-hir-compiler
  :depends-on (:sicl-conditionals-support
	       :sicl-clos-package
	       :sicl-cons-package
	       :sicl-environment
	       :sicl-simple-environment
	       :sicl-evaluation-and-compilation
	       :sicl-data-and-control-flow
	       :sicl-reader-simple
	       :cleavir-hir
	       :cleavir-ast-to-hir
	       :sicl-extrinsic-environment
	       :cleavir-hir-transformations
	       :cleavir-basic-blocks)
  :serial t
  :components
  ((:file "packages")
   (:file "host-cl-package")
   (:file "define-global-environment")
   (:file "create-global-environment")
   (:file "runtime-environment")
   (:file "import-from-host")
   (:file "import-from-conditionals")
   (:file "import-from-cleavir-env")
   (:file "standard-generic-function")
   (:file "fill-global-environment")
   (:file "parse-arguments")
   (:file "translate-hir")
   (:file "eval")
   (:file "load")
   (:file "load-files")
   (:file "repl")))
