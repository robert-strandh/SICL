(cl:in-package #:asdf-user)

(defsystem :sicl-format-test
  :depends-on (:lisp-unit :sicl-format)
  :serial t
  :components
  ((:file "format-test")))
