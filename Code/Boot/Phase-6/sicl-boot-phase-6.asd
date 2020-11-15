(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-6
  :depends-on (#:sicl-boot-base
               #:sicl-clos-boot-support)
  :serial t
  :components
  ((:file "packages")
   (:file "enable-method-combinations")
   (:file "enable-generic-function-creation")
   (:file "copy-classes")
   (:file "update-class-slots")
   (:file "boot")))
