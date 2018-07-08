(cl:in-package #:asdf-user)

(defsystem :sicl-allocator
  :depends-on (:sicl-gc-memory)
  :serial t
  :components
  ((:file "packages")
   (:file "allocator")))
