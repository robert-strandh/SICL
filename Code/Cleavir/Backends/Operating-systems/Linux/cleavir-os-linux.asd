(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-os-linux
  :serial t
  :components
  ((:file "packages")
   (:file "classes")))
