(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-processor-x86-64
  :serial t
  :components
  ((:file "packages")
   (:file "generic-functions")
   (:file "classes")))
