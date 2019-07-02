(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-2
  :depends-on (#:sicl-boot-base)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "set-up-environments")
   (:file "enable-defgeneric")
   (:file "load-accessor-defgenerics")
   (:file "enable-defclass")
   (:file "load-mop-defclasses")
   (:file "boot")))
