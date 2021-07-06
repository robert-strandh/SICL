(cl:in-package #:asdf-user)

(defsystem :sicl-memory
  :serial t
  :components
  ((:file "packages")
   (:file "image")
   (:file "object-fixnum")))
