(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-format
    :components
  ((:file "packages" :depends-on ())
   (:file "format" :depends-on ("packages"))))
