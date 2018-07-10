(cl:in-package #:asdf-user)

(defsystem :sicl-allocator-test
  :depends-on (:sicl-allocator)
  :serial t
  :components
  ((:file "packages")
   (:file "allocator-test")))
