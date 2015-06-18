(cl:in-package #:asdf-user)

(defsystem :cleavir-liveness
  :depends-on (:cleavir-utilities)
  :components
  ((:file "packages")
   (:file "liveness" :depends-on ("packages"))))
