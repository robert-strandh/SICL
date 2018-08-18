(cl:in-package #:asdf-user)

(defsystem #:sicl-new-boot
  :depends-on (#:sicl-new-boot-phase-1
               #:sicl-new-boot-phase-2)
  :serial t
  :components
  ((:file "boot")))
