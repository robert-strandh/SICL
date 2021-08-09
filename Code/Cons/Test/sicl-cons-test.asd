(cl:in-package #:asdf-user)

(defsystem :sicl-cons-test
  :depends-on (:sicl-cons)
  :serial t
  :components
  ((:file "packages")
   (:file "test")))
