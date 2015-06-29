(cl:in-package #:asdf-user)

(defsystem :cleavir-generate-ast
  :depends-on (:cleavir-ast
	       :cleavir-ast-transformations
	       :cleavir-primop
	       :cleavir-code-utilities
	       :cleavir-environment
	       :cleavir-internationalization)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "condition-reporters-english")
   (:file "source-tracking")
   (:file "destructuring")
   (:file "check-special-form-syntax")
   (:file "environment-query")
   (:file "utilities")
   (:file "minimal-compilation")
   (:file "generate-ast")
   (:file "convert-form")
   (:file "convert-special")
   (:file "convert-primop")
   (:file "ast-from-file")))
