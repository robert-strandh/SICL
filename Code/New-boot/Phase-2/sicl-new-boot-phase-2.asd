(cl:in-package #:asdf-user)

(defsystem "sicl-new-boot-phase-2"
  :depends-on ("sicl-new-boot-phase-1")
  :serial t
  :components
  ((:file "packages")
   (:file "client")
   (:file "environment")
   (:file "configuration")
   (:file "import-from-host")
   (:file "define-ensure-generic-function")
   (:file "define-ensure-class")
   (:file "define-ensure-method-combination-template")
   (:file "define-find-method-combination-template")
   (:file "define-ecclesia-functions")
   (:file "boot")))
