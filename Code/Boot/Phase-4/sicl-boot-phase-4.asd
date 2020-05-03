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
   (:file "finalize-all-classes")
   (:file "enable-allocate-instance")
   (:file "define-class-of")
   (:file "define-stamp")
   (:file "define-compile")
   (:file "enable-class-initialization")
   (:file "boot")))
