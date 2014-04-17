(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-mir-interpreter
  :depends-on (:cleavir-mir)
  :serial t
  :components
  ((:file "packages")
   (:file "general")))
