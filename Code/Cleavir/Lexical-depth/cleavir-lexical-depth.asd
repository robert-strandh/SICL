(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-lexical-depth
  :depends-on (:cleavir-mir)
  :serial t
  :components
  ((:file "packages")
   (:file "lexical-depths")))
