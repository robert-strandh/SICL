(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-5
  :depends-on (#:sicl-boot-base
               #:sicl-clos-boot-support)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   ;; (:file "utilities")
   (:file "enable-class-finalization")
   (:file "enable-defmethod-in-e4")
   (:file "enable-allocate-instance-in-e4")
   (:file "enable-object-initialization")
   (:file "enable-generic-function-invocation-in-e5")
   (:file "enable-defgeneric-in-e6")
   (:file "define-accessor-generic-functions-in-e6")
   (:file "enable-class-initialization-in-e5")
   (:file "define-mop-classes")
   (:file "enable-method-combinations-in-e5")
   (:file "boot-phase-5")))
