(cl:in-package #:asdf-user)

(defsystem :sicl-boot
  :depends-on (:sicl-extrinsic-environment)
  :serial t
  :components
  ((:file "packages")
   (:file "boot")))
