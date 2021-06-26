(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-3
  :depends-on (#:sicl-boot-base)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "enable-typep")
   (:file "enable-object-creation")
   (:file "enable-method-combinations")
   (:file "enable-defgeneric")
   (:file "enable-defmethod")
   (:file "prepare-next-phase")
   (:file "ast-evaluator-configuration")
   (:file "boot")))
