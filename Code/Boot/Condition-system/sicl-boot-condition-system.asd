(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-condition-system
  :depends-on (#:sicl-boot-phase-5)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "boot")))
