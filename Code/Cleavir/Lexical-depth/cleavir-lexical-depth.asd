(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-lexical-depth
  :depends-on (:cleavir-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "lexical-depths")))
