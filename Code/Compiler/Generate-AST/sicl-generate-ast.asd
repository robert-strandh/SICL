(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-generate-ast
  :depends-on (:cleavir-ast
	       :cleavir-primop)
  :serial t
  :components
  ((:file "packages")
   (:file "cons-packages")
   (:file "generate-ast")
   (:file "cons")))

