(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-eliminate-typeq
  :depends-on (:cleavir-mir)
  :serial t
  :components
  ((:file "packages")
   (:file "eliminate-typeq")))
