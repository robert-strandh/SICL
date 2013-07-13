(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-compiler-basic-blocks
  :depends-on (:sicl-compiler-utilities)
  :components
  ((:file "packages")
   (:file "basic-blocks" :depends-on ("packages"))))
