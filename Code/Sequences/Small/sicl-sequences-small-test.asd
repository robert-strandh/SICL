(cl:in-package #:asdf-user)

(defsystem :sicl-sequences-small-test
  :depends-on (:lisp-unit :sicl-sequences-small)
  :components
  ((:file "test" :depends-on ())))
