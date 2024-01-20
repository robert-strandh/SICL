(cl:in-package #:asdf-user)

(defsystem "sicl-new-boot"
  :depends-on ("sicl-new-boot-shared"
               "sicl-new-boot-phase-1")
  :components
  ((:file "boot")))
