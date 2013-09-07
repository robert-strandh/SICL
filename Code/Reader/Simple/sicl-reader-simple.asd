(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-reader-simple
  :depends-on (:sicl-code-utilities)
  :components
  ((:file "packages")
   (:file "more-variables"
    :depends-on ("packages"))
   (:file "additional-conditions"
    :depends-on ("packages"))
   (:file "readtable"
    :depends-on ("packages" "more-variables"))
   (:file "tokens"
    :depends-on ("packages" "readtable" "more-variables"))
   (:file "reader"
    :depends-on ("packages" "readtable" "tokens" "more-variables"))
   (:file "macro-functions"
    :depends-on ("packages" "more-variables"))
   (:file "init"
    :depends-on ("readtable" "macro-functions"))))

