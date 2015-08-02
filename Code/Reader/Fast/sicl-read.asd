(cl:in-package #:asdf-user)

(defsystem :sicl-read
  :serial t
  :components
  ((:file "packages")
   (:file "float")
   (:file "read")))
