(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-extrinsic-environment
  :depends-on (:cleavir-environment
	       :cleavir-generate-ast
	       :cleavir-ast-interpreter
	       :sicl-simple-environment)
  :serial t
  :components
  ((:file "packages")
   (:file "customization")
   (:file "create")
   (:file "fill")))
