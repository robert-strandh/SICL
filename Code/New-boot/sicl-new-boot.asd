(cl:in-package #:asdf-user)

(defsystem "sicl-new-boot"
  :depends-on ("sicl-new-boot-shared"
               "sicl-new-boot-phase-1"
               "sicl-new-boot-phase-2"
               "sicl-new-boot-phase-3"
               "sicl-new-boot-phase-4"
               "sicl-new-boot-phase-5"
               "sicl-new-boot-phase-6")
  :components
  ((:file "boot")))
