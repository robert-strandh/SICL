(in-package #:common-lisp-user)

(asdf:defsystem cleavir-code-utilities
  :serial t
  :components
  ((:file "packages")
   (:file "general")
   (:file "declarations")
   (:file "lambda-lists")))
