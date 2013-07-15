(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-compiler-reaching-definitions
  :depends-on (:sicl-compiler-utilities)
  :components
  ((:file "packages")
   (:file "reaching-definitions" :depends-on ("packages"))))
