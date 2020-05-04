(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-4
  :depends-on (#:sicl-boot-base
               #:sicl-clos-boot-support)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "import-from-host")
   (:file "utilities")
   (:file "define-stamp")
   (:file "define-compile")
   (:file "boot")))
