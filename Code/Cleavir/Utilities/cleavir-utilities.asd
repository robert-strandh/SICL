(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-utilities
  :components
  ((:file "packages")
   (:file "utilities" :depends-on ("packages"))))
