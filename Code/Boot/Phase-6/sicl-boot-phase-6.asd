(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-6
  :depends-on (#:sicl-boot-base
               #:sicl-clos-boot-support)
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "environment")
   (:file "enable-class-finalization")
   (:file "finalize-all-classes")
   (:file "enable-defmethod")
   (:file "enable-allocate-instance")
   (:file "enable-object-initialization")
   (:file "enable-method-combinations")
   (:file "enable-generic-function-invocation")
   (:file "define-accessor-generic-functions")
   (:file "enable-class-initialization")
   (:file "create-mop-classes")
   (:file "boot")))
