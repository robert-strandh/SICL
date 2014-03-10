(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-environment
  :depends-on (:sicl-code-utilities)
  :components
  ((:file "packages")
   (:file "type-proclamations"
    :depends-on ("packages"))
   (:file "environment"
    :depends-on ("packages" "type-proclamations"))
   (:file "macroexpand"
    :depends-on ("environment"))
   (:file "type-expand"
    :depends-on ("environment"))
   (:file "fdefinition"
    :depends-on ("environment"))
   (:file "special-operator-p"
    :depends-on ("environment"))
   (:file "macro-function"
    :depends-on ("environment"))
   (:file "fully-expand-form"
    :depends-on ("environment" "macro-function"))
   (:file "symbol-value"
    :depends-on ("environment"))
   (:file "constantp"
    :depends-on ("environment"))
   (:file "proclaim"
    :depends-on ("environment"))))

