(in-package #:common-lisp-user)

(asdf:defsystem #:sicl-additional-types
  :components
  ((:file "packages")
   (:file "types" :depends-on ("packages"))))
