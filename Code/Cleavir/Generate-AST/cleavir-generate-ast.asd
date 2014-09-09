(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-generate-ast
  :depends-on (:cleavir-ast
	       :cleavir-primop
	       :cleavir-code-utilities
	       :cleavir-environment)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "generate-ast")
   (:file "convert-form")))
