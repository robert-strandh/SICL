(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-compiler-def-use-chains
  :depends-on (:sicl-compiler-reaching-definitions)
  :components
  ((:file "packages")
   (:file "def-use-chains" :depends-on ("packages"))))
