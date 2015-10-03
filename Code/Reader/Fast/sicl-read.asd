(cl:in-package #:asdf-user)

(defsystem :sicl-read
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "float")
   (:file "read")))
