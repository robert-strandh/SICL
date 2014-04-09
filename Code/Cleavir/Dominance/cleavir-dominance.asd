(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-dominance
  :depends-on (:cleavir-utilities)
  :components
  ((:file "packages" :depends-on ())
   (:file "dominance" :depends-on ("packages"))))
