(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-6
  :depends-on (#:sicl-boot-base
               #:sicl-clos-boot-support)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "utilities")
   (:file "import-from-host")
   (:file "enable-allocate-instance")
   (:file "define-stamp")
   (:file "define-compile")
   (:file "boot")))
