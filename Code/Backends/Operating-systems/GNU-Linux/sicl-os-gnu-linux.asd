(cl:in-package #:asdf-user)

(defsystem :sicl-os-gnu-linux
  :serial t
  :components
  ((:file "packages")
   (:file "gnu-linux")))
