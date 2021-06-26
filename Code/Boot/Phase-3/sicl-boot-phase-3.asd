(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-3
  :depends-on (#:sicl-boot-base)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "enable-typep")
   (:file "prepare-next-phase")
   (:file "ast-evaluator-configuration")
   (:file "boot")))
