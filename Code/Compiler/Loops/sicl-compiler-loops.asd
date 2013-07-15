(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-compiler-loops
  :depends-on (:sicl-compiler-utilities)
  :components
  ((:file "packages")
   (:file "loops" :depends-on ("packages"))))
