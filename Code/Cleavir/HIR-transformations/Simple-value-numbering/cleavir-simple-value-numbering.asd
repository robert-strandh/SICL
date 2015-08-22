(cl:in-package #:asdf-user)

(defsystem :cleavir-simple-value-numbering
  :depends-on (:cleavir-meter)
  :serial t
  :components
  ((:file "packages")
   (:file "simple-value-numbering")))
