(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-read-test
  :depends-on (:lisp-unit :sicl-read)
  :components
  ((:file "test" :depends-on ())))
