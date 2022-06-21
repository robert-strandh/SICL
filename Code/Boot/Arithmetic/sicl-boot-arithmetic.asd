(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-arithmetic
  :depends-on (#:sicl-boot-phase-5)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "boot")))
