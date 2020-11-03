(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-4
  :depends-on (#:sicl-boot-base
               #:sicl-clos-boot-support)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "enable-object-creation")
   (:file "enable-method-combinations")
   (:file "enable-defgeneric")
   (:file "enable-defclass")
   (:file "enable-defmethod")
   (:file "prepare-next-phase")
   (:file "boot")))
