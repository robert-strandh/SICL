(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-7
  :depends-on (#:sicl-boot-base)
  :serial t
  :components
  ((:file "packages")
   (:file "load-make-instance")
   (:file "satiate-generic-functions")
   (:file "patch-classes")
   (:file "patch-generic-functions")
   (:file "move-functions")
   (:file "boot")))
