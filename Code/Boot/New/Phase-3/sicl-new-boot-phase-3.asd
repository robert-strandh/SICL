(cl:in-package #:asdf-user)

(defsystem #:sicl-new-boot-phase-3
  :depends-on (#:sicl-new-boot-base)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "set-up-environments")
   (:file "boot")))
