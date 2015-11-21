(cl:in-package #:asdf-user)

(defsystem :sicl-conditions
  :serial t
  :components
  ((:file "packages")
   (:file "support")))
