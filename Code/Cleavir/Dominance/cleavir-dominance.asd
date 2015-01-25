(cl:in-package #:asdf-user)

(defsystem :cleavir-dominance
  :depends-on (:cleavir-utilities)
  :components
  ((:file "packages" :depends-on ())
   (:file "dominance" :depends-on ("packages"))))
