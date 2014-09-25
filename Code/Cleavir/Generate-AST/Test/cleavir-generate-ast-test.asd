(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-generate-ast-test
  :depends-on (:cleavir-generate-ast)
  :serial t
  :components
  ((:file "minimal-compilation")
   (:file "generate-ast")))

