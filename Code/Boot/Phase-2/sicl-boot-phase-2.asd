(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-2
  :depends-on (#:sicl-boot-base)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "import-from-host")
   (:file "add-readers-and-writers")
   (:file "enable-typep")
   (:file "enable-object-creation")
   (:file "enable-defgeneric")
   (:file "enable-class-initialization")
   (:file "prepare-next-phase")
   (:file "boot")))
