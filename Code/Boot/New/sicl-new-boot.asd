(cl:in-package #:asdf-user)

(defsystem #:sicl-new-boot
  :depends-on (#:sicl-hir-to-cl
               #:sicl-new-boot-phase-0
               #:sicl-new-boot-phase-1
               #:sicl-new-boot-phase-2
               #:sicl-new-boot-phase-3
               #:sicl-new-boot-phase-4)
  :serial t
  :components
  ((:file "packages")
   (:file "boot")))
