(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-5
  :depends-on (#:sicl-boot-base
               #:sicl-clos-boot-support)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "utilities")
   (:file "import-from-host")
   (:file "enable-class-finalization")
   (:file "finalize-all-classes")
   (:file "enable-defmethod")
   (:file "enable-allocate-instance")
   (:file "enable-object-initialization")
   (:file "enable-generic-function-invocation")
   (:file "enable-class-initialization")
   (:file "boot")))
