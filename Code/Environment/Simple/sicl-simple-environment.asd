(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-simple-environment
  :depends-on (:sicl-environment)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")))
