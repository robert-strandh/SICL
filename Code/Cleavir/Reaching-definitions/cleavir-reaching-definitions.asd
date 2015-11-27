(cl:in-package #:asdf-user)

(defsystem :cleavir-reaching-definitions
  :depends-on (:cleavir-utilities)
  :serial t
  :components
  ((:file "packages")
   (:file "reaching-definitions")))
