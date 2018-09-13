(cl:in-package #:asdf-user)

(defsystem cleavir-macroexpand-all
  :depends-on (:cleavir-primop
	       :cleavir-code-utilities
	       :cleavir-environment
	       :cleavir-compilation-policy)
  :serial t
  :components
  ((:file "packages")
   (:file "environment-query")
   (:file "expand")
   (:file "expand-special")))
