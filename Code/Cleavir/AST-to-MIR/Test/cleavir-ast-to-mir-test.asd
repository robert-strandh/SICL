(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-ast-to-mir-test
  :depends-on (:cleavir-generate-ast-test
	       :cleavir-ast-to-mir
	       :cleavir-mir-interpreter)
  :serial t
  :components
  ((:file "packages")
   (:file "ast-to-mir")))
