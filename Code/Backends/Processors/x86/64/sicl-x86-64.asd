(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-x86-64
  :depends-on (:cleavir-processor-x86-64)
  :serial t
  :components
  ((:file "packages")
   (:file "backend")
   (:file "registers")))
