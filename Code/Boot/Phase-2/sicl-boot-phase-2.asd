(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-2
  :depends-on (#:sicl-boot-base)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "utilities")
   (:file "header")
   (:file "enable-method-combinations-in-e2")
   (:file "enable-generic-function-invocation-in-e2")
   (:file "enable-defmethod-in-e2")
   (:file "enable-defgeneric-in-e3")
   (:file "define-accessor-generic-functions-in-e3")
   (:file "enable-class-initialization-in-e2")
   (:file "define-mop-classes")
   (:file "boot-phase-2")))
