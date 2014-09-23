(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-code-walker
  :depends-on (:cleavir-code-utilities
	       :cleavir-environment)
  :serial t
  :components
  ((:file "packages")))
