(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-conditions
  :components
  ((:file "packages")
   (:file "support"
    :depends-on ("packages"))))
