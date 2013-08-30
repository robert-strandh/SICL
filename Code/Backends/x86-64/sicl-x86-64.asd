(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-x86-64
  :depends-on (:sicl-compiler)
  :components
  ((:file "packages")
   (:file "backend")
   (:file "ast-transformations" :depends-on ("packages" "backend"))
   (:file "lir" :depends-on ("packages" "backend"))))
