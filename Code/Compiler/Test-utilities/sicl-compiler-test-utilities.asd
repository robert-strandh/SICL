(cl:in-package #:asdf-user)

(defsystem :sicl-compiler-test-utilities
  :depends-on (:sicl-compiler-utilities)
  :serial t
  :components
  ((:file "packages")
   (:file "test-utilities")))
