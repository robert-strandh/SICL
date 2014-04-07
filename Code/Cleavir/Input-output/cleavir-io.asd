(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-io
  :serial t
  :components
  ((:file "packages")
   (:file "io")))
