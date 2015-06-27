(cl:in-package #:asdf-user)

(defsystem :sicl-format
    :components
  ((:file "packages" :depends-on ())
   (:file "format" :depends-on ("packages"))))
