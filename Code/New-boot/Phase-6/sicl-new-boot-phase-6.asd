(cl:in-package #:asdf-user)

(defsystem "sicl-new-boot-phase-6"
  :depends-on ()
  :serial t
  :components
  ((:file "packages")
   (:file "client")
   (:file "configuration")
   (:file "boot")))
