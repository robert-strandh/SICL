(in-package #:cl-user)

(asdf:defsystem :sicl-loop
  :depends-on ("sicl-additional-conditions")
  :components
  ((:file "packages" :depends-on ())
   (:file "loop" :depends-on ("packages"))))
