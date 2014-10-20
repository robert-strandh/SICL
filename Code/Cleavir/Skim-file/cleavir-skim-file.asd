(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-skim-file
  :depends-on (:cleavir-environment)
  :serial t
  :components
  ((:file "packages")
   (:file "skim-file")))
