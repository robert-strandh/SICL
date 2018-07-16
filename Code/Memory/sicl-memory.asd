(cl:in-package #:asdf-user)

(defsystem :sicl-memory
  :components
  ((:file "packages")
   (:file "image" :depends-on ("packages"))))
