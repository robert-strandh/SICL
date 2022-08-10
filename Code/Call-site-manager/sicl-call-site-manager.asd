(cl:in-package #:asdf-user)

(defsystem #:sicl-call-site-manager
  :depends-on (#:sicl-environment)
  :serial t
  :components
  ((:file "packages")))
