(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-fill-target-environment
  :depends-on (#:sicl-boot-phase-5)
  :serial t
  :components
  ((:file "packages")
   (:file "client")
   (:file "boot")))
