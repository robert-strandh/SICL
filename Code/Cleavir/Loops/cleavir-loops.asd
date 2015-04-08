(cl:in-package #:asdf-user)

(defsystem :cleavir-loops
  :depends-on (:cleavir-utilities)
  :components
  ((:file "packages")
   (:file "loops" :depends-on ("packages"))))
