(cl:in-package #:asdf-user)

(defsystem #:sicl-boot
  :depends-on (#:sicl-hir-evaluator
               #:sicl-boot-phase-0
               #:sicl-boot-phase-1
               #:sicl-boot-phase-2
               #:sicl-boot-phase-3
               #:sicl-boot-phase-4
               #:sicl-boot-phase-5
               #:sicl-boot-phase-6
               #:sicl-boot-phase-7
               #:sicl-boot-phase-8)
  :serial t
  :components
  ((:file "packages")
   (:file "boot")))
