(in-package #:asdf-user)

(defsystem #:sicl-additional-types
  :components
  ((:file "packages")
   (:file "types" :depends-on ("packages"))))
