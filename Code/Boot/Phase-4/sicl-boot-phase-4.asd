(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-4
  :depends-on (#:sicl-ast-compiler
               #:sicl-boot-base
               #:sicl-clos-boot-support)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "ast-evaluator-configuration")
   (:file "boot")))
