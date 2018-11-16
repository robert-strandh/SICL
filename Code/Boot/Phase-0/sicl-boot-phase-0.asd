(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-0
  :depends-on (#:sicl-boot-base)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "utilities")
   (:file "boot-phase-0")))
