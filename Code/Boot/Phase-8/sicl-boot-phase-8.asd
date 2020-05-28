(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-8
  :depends-on (#:sicl-boot-base)
  :serial t
  :components
  ((:file "packages")
   (:file "boot")))
