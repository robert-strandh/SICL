(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-environment
  :depends-on (:sicl-code-utilities)
  :components
  ((:file "packages")
   (:file "type-proclamations"
    :depends-on ("packages"))
   (:file "environment"
    :depends-on ("packages" "type-proclamations"))
   (:file "environment-query"
    :depends-on ("environment"))
   (:file "macroexpand"
    :depends-on ("environment-query"))
   (:file "type-expand"
    :depends-on ("environment-query"))
   (:file "fdefinition"
    :depends-on ("environment-query"))
   (:file "special-operator-p"
    :depends-on ("environment-query"))
   (:file "macro-function"
    :depends-on ("environment-query"))
   (:file "fully-expand-form"
    :depends-on ("environment-query" "macro-function"))
   (:file "symbol-value"
    :depends-on ("environment-query"))
   (:file "constantp"
    :depends-on ("environment-query"))
   (:file "proclaim"
    :depends-on ("environment-query"))))

