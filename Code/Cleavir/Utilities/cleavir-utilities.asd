(cl:in-package #:asdf-user)

(defsystem :cleavir-utilities
  :components
  ((:file "packages")
   (:file "utilities" :depends-on ("packages"))))
