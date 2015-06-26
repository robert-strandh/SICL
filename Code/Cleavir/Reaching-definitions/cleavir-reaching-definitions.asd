(cl:in-package #:asdf-user)

(defsystem :cleavir-reaching-definitions
  :depends-on (:cleavir-utilities)
  :components
  ((:file "packages")
   (:file "reaching-definitions" :depends-on ("packages"))))
