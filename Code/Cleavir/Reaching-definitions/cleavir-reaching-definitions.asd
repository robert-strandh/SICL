(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-reaching-definitions
  :depends-on (:cleavir-utilities)
  :components
  ((:file "packages")
   (:file "reaching-definitions" :depends-on ("packages"))))
