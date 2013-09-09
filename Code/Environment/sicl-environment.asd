(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-environment
  :depends-on (:sicl-code-utilities)
  :components
  ((:file "packages")
   (:file "type-proclamations"
    :depends-on ("packages"))
   (:file "environment"
    :depends-on ("packages" "type-proclamations"))))
