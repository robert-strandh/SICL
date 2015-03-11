(cl:in-package #:asdf-user)

(defsystem :sicl-target-sicl
  :serial t
  :components
  ((:file "packages")
   (:file "target")))
