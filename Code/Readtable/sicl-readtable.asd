(cl:in-package #:asdf-user)

(defsystem :sicl-readtable
  :serial t
  :components
  ((:file "packages")
   (:file "generic-functions")))
