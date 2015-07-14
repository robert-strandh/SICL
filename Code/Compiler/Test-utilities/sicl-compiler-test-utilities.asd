(cl:in-package #:asdf-user)

(defsystem :sicl-compiler-test-utilities
  :depends-on (:sicl-compiler-utilities)
  :components
  ((:file "packages")
   (:file "test-utilities" :depends-on ("packages"))))
