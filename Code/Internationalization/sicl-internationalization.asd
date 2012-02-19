(in-package #:common-lisp-user)

(asdf:defsystem #:sicl-internationalization
    :components
  ((:file "packages")
   (:file "locale" :depends-on ("packages"))))
