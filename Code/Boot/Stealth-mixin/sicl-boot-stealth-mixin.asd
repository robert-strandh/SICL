(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-stealth-mixin
  :depends-on (#:sicl-boot-phase-5)
  :serial t
  :components
  ((:file "packages")
   (:file "boot")))
