(cl:in-package #:asdf-user)

(defsystem "sicl-new-boot-phase-5"
  :depends-on ()
  :serial t
  :components
  ((:file "packages")
   (:file "client")
   (:file "configuration")
   (:file "satiation")
   (:file "tie-the-knot")
   (:file "set-default-discriminating-function")
   (:file "boot")))
