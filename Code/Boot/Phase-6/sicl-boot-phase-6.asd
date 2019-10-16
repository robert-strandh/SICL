(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-6
  :depends-on (#:sicl-boot-base)
  :serial t
  :components
  ((:file "packages")
   (:file "load-cons-related-functions")
   (:file "boot")))
