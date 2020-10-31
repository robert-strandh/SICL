(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-1
  :depends-on (#:sicl-boot-base)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "import-from-host")
   (:file "add-readers-and-writers")
   (:file "enable-defgeneric")
   (:file "enable-defmethod")
   (:file "enable-class-initialization")
   (:file "enable-defclass")
   (:file "prepare-next-phase")
   (:file "boot")))
