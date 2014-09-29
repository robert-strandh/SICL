(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-ast-interpreter
  :depends-on (:cleavir-ast)
  :serial t
  :components
  ((:file "packages")
   (:file "interpreter")))
