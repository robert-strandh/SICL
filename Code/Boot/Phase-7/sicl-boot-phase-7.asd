(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-7
  :depends-on (#:sicl-boot-base)
  :serial t
  :components
  ((:file "packages")
   (:file "check-environment")))
