(cl:in-package #:asdf-user)

(defsystem :cleavir-io
  :depends-on (#:clonedijk)
  :serial t
  :components
  ((:file "packages")
   (:file "io")))
