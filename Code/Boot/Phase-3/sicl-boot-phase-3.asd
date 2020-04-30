(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-3
  :depends-on (#:sicl-boot-base
               #:sicl-clos-boot-support)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "utilities")
   (:file "set-up-environments")
   (:file "header")
   (:file "HIR-interpreter-configuration")
   (:file "define-make-instance")
   (:file "enable-defmethod")
   (:file "define-method-on-method-function")
   (:file "enable-generic-function-invocation")
   (:file "define-accessor-generic-functions")
   (:file "enable-class-initialization")
   (:file "boot")))
