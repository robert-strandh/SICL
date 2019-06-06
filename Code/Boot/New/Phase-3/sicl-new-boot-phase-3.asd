(cl:in-package #:asdf-user)

(defsystem #:sicl-new-boot-phase-3
  :depends-on (#:sicl-new-boot-base)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "utilities")
   (:file "set-up-environments")
   (:file "header")
   (:file "define-make-instance")
   (:file "enable-defmethod")
   (:file "enable-method-combinations")
   (:file "define-method-on-method-function")
   (:file "enable-generic-function-invocation")
   (:file "boot")))
