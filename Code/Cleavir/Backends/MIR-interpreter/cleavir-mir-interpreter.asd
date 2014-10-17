(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-mir-interpreter
  :depends-on (:cleavir-mir
	       :cleavir-lexical-depth)
  :serial t
  :components
  ((:file "packages")
   (:file "translate")
   (:file "environment")
   (:file "accessors")))
