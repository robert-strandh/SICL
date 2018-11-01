(cl:in-package #:asdf-user)

(defsystem #:sicl-new-boot-phase-2
  :depends-on (#:sicl-new-boot-base)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "utilities")
   (:file "enable-generic-function-invocation-in-e2")
   (:file "enable-defmethod-in-e2")
   (:file "define-accessor-generic-functions-in-e3")
   (:file "define-mop-classes")
   (:file "boot-phase-2")))
