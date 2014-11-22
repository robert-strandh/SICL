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
   (:file "check-special-form-syntax")
   (:file "environment-query")
   (:file "utilities")
   (:file "minimal-compilation")
   (:file "generate-ast")
   (:file "convert-form")
   (:file "convert-special")
   (:file "convert-primop")))
