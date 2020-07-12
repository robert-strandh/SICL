(cl:in-package #:asdf-user)

(defsystem :sicl-conditions
  :serial t
  :components
  ((:file "packages")
   (:file "support")
   (:file "condition-class-defclass")
   (:file "condition-defclass")
   (:file "define-condition-support")
   (:file "define-condition-defmacro")
   (:file "Portable-condition-system/condition-hierarchy")
   (:file "check-type-defmacro")
   (:file "Portable-condition-system/restart-defclass")
   (:file "Portable-condition-system/restarts-utilities")
   (:file "Portable-condition-system/restarts")))
