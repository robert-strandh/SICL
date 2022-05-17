(cl:in-package #:asdf-user)

(defsystem #:sicl-boot
  :depends-on (#:sicl-boot-base
               #:sicl-boot-phase-1
               #:sicl-boot-phase-2
               #:sicl-boot-phase-3
               #:sicl-boot-phase-4
               #:sicl-boot-phase-5
               #:sicl-boot-alexandria
               #:sicl-boot-sequence-functions
               #:sicl-boot-stealth-mixin
               #:sicl-boot-trucler
               #:sicl-boot-environment
               #:sicl-boot-client
               #:sicl-boot-phase-6
               #:sicl-boot-phase-7
               #:sicl-boot-condition-system)
  :serial t
  :components
  ((:file "boot")
   (:file "trace")))
