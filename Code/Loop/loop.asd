(in-package #:cl-user)

(asdf:defsystem :sicl-loop
  :depends-on ("sicl-additional-conditions")
  :components
  ((:file "packages" :depends-on ())
   (:file "loop" :depends-on ("packages"))))

(asdf:defsystem :sicl-loop-test
  :depends-on (:sicl-loop)
  :components
  ((:file "loop-test" :depends-on ())))
