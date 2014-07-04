(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-simplify-box-unbox
  :depends-on (:cleavir-mir)
  :components
  ((:file "packages")
   (:file "simplify-box-unbox")))
