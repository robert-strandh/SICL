(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-ast-to-mir-test
  :depends-on (:cleavir-ast-to-mir
	       :cleavir-mir-interpreter)
  :serial t
  :components
  ((:file "packages")))
