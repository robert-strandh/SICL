(cl:in-package #:asdf-user)

(defsystem #:sicl-new-boot
  :depends-on (#:sicl-new-boot-phase-1)
  :serial t)
