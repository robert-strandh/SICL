(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-loops
  :depends-on (:cleavir-utilities)
  :components
  ((:file "packages")
   (:file "loops" :depends-on ("packages"))))
