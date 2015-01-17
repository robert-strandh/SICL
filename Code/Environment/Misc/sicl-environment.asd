(cl:in-package #:asdf-user)

(defsystem :sicl-environment
  :depends-on (:sicl-global-environment)
  :serial t
  :components
  ((:file "packages")))
