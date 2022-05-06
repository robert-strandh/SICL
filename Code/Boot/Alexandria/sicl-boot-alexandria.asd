(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-alexandria
  :depends-on (#:sicl-boot-phase-5)
  :serial t
  :components
  ((:file "packages")
   (:file "boot")))
