(cl:in-package #:asdf-user)

(defsystem :sicl-read
    :components
  ((:file "packages" :depends-on ())
   (:file "float" :depends-on ("packages"))
   (:file "read" :depends-on ("packages" "float"))))
