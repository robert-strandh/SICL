(cl:in-package #:asdf-user)

(defsystem #:fast-generic-functions
  :serial t
  :components
  ((:file "packages")
   (:file "configuration")))
