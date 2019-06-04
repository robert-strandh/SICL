(cl:in-package #:asdf-user)

(defsystem #:sicl-new-boot-phase-1
  :depends-on ()
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "boot-phase-1")))
