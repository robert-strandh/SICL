(cl:in-package #:asdf-user)

(defsystem :sicl-simple-environment
  :depends-on (:sicl-global-environment)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "methods")))
