(cl:in-package #:asdf-user)

(defsystem :cleavir2-lir
  :depends-on (:cleavir2-ir)
  :serial t
  :components
  ((:file "general")))
