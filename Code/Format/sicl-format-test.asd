(cl:in-package #:asdf-user)

(defsystem :sicl-format-test
  :depends-on (:lisp-unit :sicl-format)
  :components
  ((:file "format-test" :depends-on ())))
