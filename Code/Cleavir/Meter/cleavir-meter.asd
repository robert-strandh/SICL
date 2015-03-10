(cl:in-package #:asdf-user)

(defsystem :cleavir-meter
  :serial t
  :components
  ((:file "packages")
   (:file "meter")))
