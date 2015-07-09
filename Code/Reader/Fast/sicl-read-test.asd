(cl:in-package #:asdf-user)

(defsystem :sicl-read-test
  :depends-on (:lisp-unit :sicl-read)
  :components
  ((:file "test" :depends-on ())))
