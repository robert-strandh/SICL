(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-5
  :depends-on (#:sicl-boot-base
               #:sicl-clos-boot-support)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "create-additional-generic-functions")
   (:file "create-additional-classes")
   (:file "hir-evaluator-configuration")
   (:file "enable-typep")
   (:file "enable-array-access")
   (:file "enable-object-creation")
   (:file "enable-method-combinations")
   (:file "enable-generic-function-creation")
   (:file "enable-printing")
   (:file "enable-defgeneric")
   (:file "enable-defmethod")
   (:file "create-cyclic-graph")
   (:file "prepare-next-phase")
   (:file "boot")))
