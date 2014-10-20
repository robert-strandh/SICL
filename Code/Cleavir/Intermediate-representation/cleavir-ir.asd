(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-ir
  :serial t
  :components
  ((:file "packages")
   (:file "general")))
