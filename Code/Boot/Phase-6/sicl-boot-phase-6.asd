(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-6
  :depends-on (#:sicl-boot-base)
  :serial t
  :components
  ((:file "packages")
   (:file "cyclify")
   (:file "boot-phase-6")))
