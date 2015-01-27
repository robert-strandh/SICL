(cl:in-package #:asdf-user)

(defsystem :cleavir-io
  :serial t
  :components
  ((:file "packages")
   (:file "io")))
