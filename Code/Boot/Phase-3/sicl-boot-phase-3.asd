(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-3
  :depends-on (#:sicl-boot-base)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "enable-typep")
   (:file "enable-object-creation")
   (:file "enable-defgeneric")
   (:file "add-readers-and-writers")
   (:file "enable-class-initialization")
   (:file "enable-defclass")
   (:file "enable-defmethod")
   (:file "prepare-this-phase")
   (:file "ast-evaluator-configuration")
   (:file "boot")))
