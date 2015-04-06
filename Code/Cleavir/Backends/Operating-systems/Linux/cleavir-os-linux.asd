(cl:in-package #:asdf-user)

(defsystem :cleavir-os-linux
  :serial t
  :components
  ((:file "packages")
   (:file "classes")))
