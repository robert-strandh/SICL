(cl:in-package #:asdf-user)

(defsystem :cleavir-simple-value-numbering
  :depends-on (:cleavir-meter :cleavir-liveness)
  :serial t
  :components
  ((:file "packages")
   (:file "meter")
   (:file "simple-value-numbering")))
