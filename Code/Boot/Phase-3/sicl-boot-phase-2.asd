(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-2
  :depends-on (#:sicl-boot-base)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "enable-method-combinations")
   (:file "enable-defgeneric")
   (:file "enable-defmethod")
   (:file "enable-defclass")
   (:file "prepare-next-phase")
   (:file "boot")))
