(cl:in-package #:asdf-user)

(defsystem :sicl-boot
  :depends-on ()
  :serial t
  :components
  ((:file "packages")
   (:file "boot")))
