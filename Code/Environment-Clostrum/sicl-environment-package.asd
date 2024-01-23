(cl:in-package #:asdf-user)

(defsystem #:sicl-environment-package
  :serial t
  :components
  ((:file "bootstrap-packages")))
