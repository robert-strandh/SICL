(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-compiler-utilities
  :components
  ((:file "packages")
   (:file "utilities" :depends-on ("packages"))))
