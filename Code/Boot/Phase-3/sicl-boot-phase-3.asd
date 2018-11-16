(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-3
  :depends-on (#:sicl-boot-base
               #:sicl-clos-boot-support)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "utilities")
   (:file "enable-class-finalization")
   (:file "enable-defmethod-in-e3")
   (:file "enable-allocate-instance-in-e2")
   (:file "enable-object-initialization")
   (:file "enable-generic-function-invocation-in-e3")
   (:file "enable-defgeneric-in-e4")
   (:file "define-accessor-generic-functions-in-e4")
   (:file "enable-class-initialization-in-e3")
   (:file "define-mop-classes")
   (:file "enable-method-combinations-in-e3")
   (:file "boot-phase-3")))
