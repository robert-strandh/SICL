(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-4
  :depends-on (#:sicl-ast-compiler
               #:sicl-boot-base
               #:sicl-clos-boot-support)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "enable-typep")
   (:file "enable-object-creation")
   (:file "enable-method-combinations")
   (:file "prepare-next-phase")
   (:file "ast-evaluator-configuration")
   (:file "boot")))
