(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-compiler-liveness
  :depends-on (:sicl-compiler-utilities)
  :components
  ((:file "packages")
   (:file "liveness" :depends-on ("packages"))))
