(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-environment
  :serial t
  :components
  ((:file "packages")))
