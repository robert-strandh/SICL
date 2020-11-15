(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-5
  :depends-on (#:sicl-boot-base
               #:sicl-clos-boot-support)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "enable-object-creation-in-e4")
   (:file "create-additional-generic-functions")
   (:file "create-additional-classes")
   (:file "hir-evaluator-configuration")
   (:file "enable-typep")
   (:file "enable-array-access")
   (:file "enable-slot-value")
   (:file "enable-object-creation")
   (:file "enable-method-combinations")
   (:file "boot")))
