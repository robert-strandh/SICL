(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-x86-64
  :depends-on (:sicl-compiler)
  :components
  ((:file "packages")
   (:file "lir" :depends-on ("packages"))))
