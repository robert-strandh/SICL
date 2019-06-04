(cl:in-package #:asdf-user)

(defsystem #:sicl-new-boot-phase-2
  :depends-on (#:sicl-new-boot-base)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "enable-defgeneric")
   (:file "boot")))
