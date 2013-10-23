(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-x86-64
  :depends-on (:sicl-compiler :sicl-boot)
  :components
  ((:file "packages")
   (:file "backend" :depends-on ("packages"))
   (:file "set-backend" :depends-on ("packages" "backend"))
   (:file "ast-transformations" :depends-on ("packages" "backend"))
   (:file "lir" :depends-on ("packages" "backend"))))
