(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-7
  :depends-on (#:sicl-boot-base)
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "load-printer")
   (:file "load-make-instance")
   (:file "satiate-generic-functions")
   (:file "patch-slot-metaobject")
   (:file "patch-classes")
   (:file "patch-method-combination")
   (:file "patch-functions-and-methods")
   (:file "move-functions")
   (:file "update-instance-slot-list")
   (:file "allocate-class-prototypes")
   (:file "enable-compilation")
   (:file "boot")))
