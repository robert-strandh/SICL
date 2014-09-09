(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-generate-ast
  :depends-on (:cleavir-ast
	       :cleavir-primop)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "generate-ast")))
