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
   (:file "finalize-all-classes")
   (:file "enable-defmethod")
   (:file "enable-allocate-instance")
   (:file "define-class-of")
   (:file "enable-object-initialization")
   (:file "define-stamp")
   (:file "define-compile")
   (:file "enable-class-initialization")
   (:file "boot")))
