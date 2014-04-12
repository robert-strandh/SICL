(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-ast-transformations
  :depdends-on (:cleavir-ast)
  :serial t
  :components
  ((:file "packages")))
