(cl:in-package #:asdf-user)

(defsystem #:sicl-new-boot-phase-4
  :depends-on (#:sicl-new-boot-base
               #:sicl-clos-boot-support)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   ;; (:file "utilities")
   (:file "enable-class-finalization")
   (:file "enable-defmethod-in-e3")
   (:file "enable-allocate-instance-in-e3")
   (:file "enable-object-initialization")
   (:file "enable-generic-function-invocation-in-e4")
   (:file "enable-defgeneric-in-e5")
   (:file "define-accessor-generic-functions-in-e5")
   (:file "enable-class-initialization-in-e4")
   (:file "define-mop-classes")
   (:file "enable-method-combinations-in-e4")
   (:file "boot-phase-4")
   ))
