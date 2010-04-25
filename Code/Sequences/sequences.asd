(in-package #:cl-user)

(asdf:defsystem :sicl-sequences
    :components
  ((:file "packages" :depends-on ())
   (:file "sequences" :depends-on ("packages"))))

(asdf:defsystem :sicl-sequences-test
  :depends-on (:lisp-unit :sicl-sequences)
  :components
  ((:file "test" :depends-on ())))
