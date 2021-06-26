(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-3
  :depends-on (#:sicl-boot-base)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "enable-defmethod")
   (:file "prepare-this-phase")
   (:file "ast-evaluator-configuration")
   (:file "boot")))
