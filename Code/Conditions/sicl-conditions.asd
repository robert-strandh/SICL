(cl:in-package #:asdf-user)

(defsystem :sicl-conditions
  :serial t
  :components
  ((:file "packages")
   (:file "condition-class-defclass")
   (:file "condition-defclass")
   (:file "define-condition-defmacro")))
