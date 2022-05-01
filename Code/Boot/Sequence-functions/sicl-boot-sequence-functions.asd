(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-sequence-functions
  :depends-on (#:sicl-boot-phase-5
               #:fast-generic-functions)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "boot")))
