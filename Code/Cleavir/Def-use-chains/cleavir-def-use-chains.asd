(cl:in-package #:asdf-user)

(defsystem :cleavir-def-use-chains
  :depends-on (:cleavir-reaching-definitions)
  :components
  ((:file "packages")
   (:file "def-use-chains" :depends-on ("packages"))))
