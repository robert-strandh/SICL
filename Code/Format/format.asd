(in-package #:cl-user)

(asdf:defsystem :sicl-format
    :components
  ((:file "packages" :depends-on ())
   (:file "format" :depends-on ("packages"))))

(asdf:defsystem :sicl-format-test
  :depends-on (:lisp-unit :sicl-format)
  :components
  ((:file "format-test" :depends-on ())))
