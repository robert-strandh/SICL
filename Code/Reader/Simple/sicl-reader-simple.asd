(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-reader-simple
  :components
  ((:file "packages")
   (:file "macro-functions" :depends-on ("packages"))
   (:file "additional-conditions" :depends-on ("packages"))
   (:file "tokens" :depends-on ("packages"))
   (:file "readtable" :depends-on ("packages"))
   (:file "reader" :depends-on ("packages" "readtable" "tokens"))
   (:file "init" :depends-on ("readtable" "macro-functions"))))

