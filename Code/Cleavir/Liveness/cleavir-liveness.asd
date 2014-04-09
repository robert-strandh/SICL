(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-liveness
  :depends-on (:cleavir-utilities)
  :components
  ((:file "packages")
   (:file "liveness" :depends-on ("packages"))))
