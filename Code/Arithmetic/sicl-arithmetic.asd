(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-arithmetic
  :components
  ((:file "packages")
   (:file "support"
    :depends-on ("packages"))))

