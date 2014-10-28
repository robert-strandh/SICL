(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-environment
  :depends-on (:sicl-code-utilities :cleavir-environment)
  :serial t
  :components
  ((:file "packages")
   (:file "generic-functions")
   (:file "info-methods")
   (:file "other-methods")
   (:file "other-functions")))
