(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-boot-phase2
  :depends-on (:sicl-code-utilities
	       :sicl-additional-conditions
	       :sicl-boot-phase1)
  :serial t
  :components
  ((:file "packages")
   (:file "rename-package-1")
   (:file "class-database")
   (:file "ensure-class")
   (:file "list-utilities")
   (:file "defclass-support")
   (:file "defclass-defmacro")
   (:file "bridge-generic-function")
   (:file "generic-function-database")
   (:file "ensure-generic-function")
   (:file "defgeneric-defmacro")
   (:file "rename-package-2")))
