(in-package #:cl-user)

(asdf:defsystem :sicl-read
    :components
  ((:file "packages" :depends-on ())
   (:file "read" :depends-on ("packages"))))

(asdf:defsystem :sicl-read-test
  :depends-on (:lisp-unit :sicl-read)
  :components
  ((:file "test" :depends-on ())))
