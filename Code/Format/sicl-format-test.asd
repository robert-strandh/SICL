(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-format-test
  :depends-on (:lisp-unit :sicl-format)
  :components
  ((:file "format-test" :depends-on ())))
