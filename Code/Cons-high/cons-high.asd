(in-package #:cl-user)

(asdf:defsystem :sicl-cons-high
    :components
  ((:file "packages" :depends-on ())
   (:file "cons-high" :depends-on ("packages"))))

(asdf:defsystem :sicl-cons-high-test
  :depends-on (:sicl-cons-high)
  :components
  ((:file "test" :depends-on ())))
