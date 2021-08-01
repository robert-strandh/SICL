(cl:in-package #:asdf-user)

(defsystem :sicl-package-support
  :serial t
  :description "Support code for SICL-specific package system"
  :depends-on (#:sicl-package-base)
  :components ((:file "packages")))
